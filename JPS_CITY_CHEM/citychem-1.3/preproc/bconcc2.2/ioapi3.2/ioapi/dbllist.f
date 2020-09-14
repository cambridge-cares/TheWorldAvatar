
        LOGICAL FUNCTION DBLLIST( ENAME, EDESC, NMAX, NCNT, LIST )

C***********************************************************************
C Version "$Id: dbllist.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C)  2015-2016 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  57
C
C  RETURNS:  TRUE for success, FALSE for failure
C            Success implies NCNT > 0 ("we actually found something")
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical name> <quoted, comma-delimited list of integers>
C       string-length( <list> <= 511
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       ENVINT, M3EXIT, STR2REAL
C
C  REVISION  HISTORY:
C       prototype 12/03/1998 by Carlie J. Coats, Jr., UNC IE:
C       adapted from "reallist.f"
C       Modified  06/2016 by CJC: bug-fixes
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: ENAME   !  environment variable for the list
        CHARACTER*(*), INTENT(IN   ) :: EDESC   !  environment variable description
        INTEGER      , INTENT(IN   ) :: NMAX    !  dimension for list
        INTEGER      , INTENT(  OUT) :: NCNT    !  actual number of entries in list
        REAL*8       , INTENT(  OUT) :: LIST( NMAX )    ! array of values found    

C...........   EXTERNAL FUNCTION:

        INTEGER, EXTERNAL :: LBLANK
        REAL*8,  EXTERNAL :: STR2DBLE

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*65535 BUF     !  buffer for environment-variable value
        CHARACTER*256   MSG     !  buffer for messages
        CHARACTER*5     PREFIX    !  buffer for checking "LIST:"
        INTEGER         ISTAT   !  return status for ENVSTR
        INTEGER         L, M    !  subscript/loop counter
        INTEGER         LO, HI  !  substring bounds
        INTEGER         J, K

C***********************************************************************
C   begin body of function  dummy

        CALL ENVSTR( ENAME, EDESC, ' ', BUF, ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            MSG = 'Could not get environment variable "'// ENAME// '"'
            CALL M3MSG2( MSG )
            DBLLIST = .FALSE.
            RETURN
        END IF

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
        DO  L = 1, NMAX
            LO = LO + LBLANK( BUF( LO: ) )
            IF ( LO .GE. 65535 )  THEN
                NCNT = L-1
                GO TO 99                !  list exhausted
            END IF
            J = INDEX( BUF(LO:), ',' )
            K = INDEX( BUF(LO:), ' ' )
            IF ( MAX( J, K ) .EQ. 0 ) THEN
                HI = 65536 - LO                   !  no more commas, blank-separators
            ELSE IF ( J .EQ. 0 ) THEN
                HI = K
            ELSE IF ( K .EQ. 0 ) THEN
                HI = J
            ELSE
                HI = MIN( J, K )
            END IF
            LIST( L ) = STR2DBLE( BUF( LO : ) )
            LO = LO + HI                !  1 col past the comma
        END DO

        IF ( LO+1 .LT. 65535 )  THEN   !  fall-through:  list done?
           IF ( BUF( LO+1: ) .NE. ' ' )  THEN
               DBLLIST = .FALSE.
               RETURN
            END IF
         END IF

99      CONTINUE        !  exit from loop
        DBLLIST = ( NCNT .GT. 0 )
        RETURN
        END FUNCTION DBLLIST
