
        LOGICAL FUNCTION STRLIST( ENAME, EDESC, NMAX, NCNT, LIST )

C***********************************************************************
C Version "$Id: strlist.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and 
C (C) 2014-2016 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  65
C
C  RETURNS:  TRUE for success, FALSE for failure
C            Success implies NCNT > 0 ("we actually found something")
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical name> <quoted, comma-delimited list of integers>
C       string-length( <list> <= 511
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       ENVINT, M3EXIT, STR2INT
C
C  REVISION  HISTORY:
C       prototype 04/15/1998 by CJC
C       Revised   02/09/1999 by CJC:  NCNT <= 0:  failure
C       Revised   02/11/2002 by CJC:  Deal with values "LIST:<list>"
C       Modified  03/2010 by CJC: F9x changes for I/O API v3.1
C       Modified  03/2014 by CJC: buffer-size 65535 to match "envgets.c" change
C       Modified  12/2015 by CJC: blank-delimited lists; termination-condition 
C       needs "LO+HI+1 .LT. 65535" (etc.) several places.
C       Modified  06/2016 by CJC: bug-fixes
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: ENAME           ! environment variable for the list
        CHARACTER*(*), INTENT(IN   ) :: EDESC           ! environment variable description
        INTEGER      , INTENT(IN   ) :: NMAX            ! dimension for list
        INTEGER      , INTENT(  OUT) :: NCNT            ! actual number of entries in list
        CHARACTER*(*), INTENT(  OUT) :: LIST( NMAX )    ! array of values found    

C...........   EXTERNAL FUNCTIONS:

        INTEGER, EXTERNAL :: LBLANK

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*65535 BUF       !  buffer for environment-variable value
        CHARACTER*256   MESG      !  buffer for error messages
        CHARACTER*5     PREFIX    !  buffer for checking "LIST:"
        INTEGER         ISTAT     !  return status for ENVSTR
        INTEGER         I, J, K, L, M   !  subscript/loop counters
        INTEGER         LO, HI    !  substring bounds
        INTEGER         LMAX      !  max substring length
        LOGICAL         EFLAG

C***********************************************************************
C   begin body of function  STRLIST

        CALL ENVSTR( ENAME, EDESC, ' ', BUF, ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Could not get environment variable "'// ENAME// '"'
            CALL M3MSG2( MESG )
            STRLIST = .FALSE.
            RETURN
        END IF
        
        LMAX = LEN( LIST( 1 ) )
        EFLAG = .FALSE.

        BUF    = ADJUSTL( BUF )
        PREFIX = BUF( 1:5 )
        CALL UPCASE( PREFIX )
        IF ( PREFIX .EQ. 'LIST:' ) THEN
            HI = 5
            LO = 6
        ELSE
            HI = 0
            LO = 1
        END IF
        DO  K = 1, NMAX
            LO = LO + LBLANK( BUF( LO: ) )
            IF ( LO .GE. 65535 ) THEN
                NCNT = K - 1
                GO TO 99                !  list exhausted
            END IF
            I = INDEX( BUF(LO:), ',' )
            J = INDEX( BUF(LO:), ' ' )
            IF ( MAX( J, K ) .EQ. 0 ) THEN
                HI = 0                   !  no more commas, blank-separators
            ELSE IF ( I .EQ. 0 ) THEN
                HI = J
            ELSE IF ( J .EQ. 0 ) THEN
                HI = I
            ELSE
                HI = MIN( I,J )
            END IF
            IF ( HI .EQ. 0 ) THEN          !  no more commas
                L  = LEN_TRIM( BUF( LO: ) )
                HI = 65536 - LO  
            ELSE        !  comma is BUF( LO+HI-1:LO+HI-1 )
                L = LEN_TRIM( BUF( LO : LO+HI-2 ) )
            END IF
            IF ( L .GT. 0  .AND. L .LE. LMAX ) THEN
                LIST( K ) = BUF( LO : LO+L-1 )
            ELSE
                EFLAG = .TRUE.
            END IF
            LO = LO + HI                !  1 col past the comma
        END DO

        IF ( LO+1 .LT. 65535 )  THEN   !  fall-through:  list done?
            IF ( BUF( LO+1: ) .NE. ' ' )  THEN   !  fall-through:  list done?
                STRLIST = .FALSE.
                RETURN
             END IF
         END IF

99      CONTINUE        !  exit from loop
        STRLIST = ( .NOT. EFLAG ) .AND. ( NCNT .GT. 0 )
        RETURN
        END FUNCTION STRLIST
