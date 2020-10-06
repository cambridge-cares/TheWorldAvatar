
        INTEGER FUNCTION GETMENU( ITEMCNT, DEFAULT, PROMPT, CHOICES )

C...............................................................
C Version "$Id: getmenu.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C       function body starts at line  87
C
C       CALLS:
C       RETURNS:    user-selected item number from menu choices.
C
C  FUNCTION:
C
C       Display a menu to the screen; then display a prompt, get the
C       user's response and check that it is within range.
C       NOTE:  prompt and menu choice strings  should have
C       length <= 72 characters.  Ideally, number of items should be <= 18;
C       should be <= 999 and >= 1 in any case.
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C
C       ITEMCNT     number of items in menu. Ideally, < 20
C       DEFAULT     default menu choice
C       PROMPT      menu prompt
C       CHOICES     array of menu choices
C
C    Output arguments:  none
C
C  REVISION HISTORY:
C
C       Created  9/1989 by CJC.
C       Modified 6/1990 by CJC for ROM 2.2 -- error exit via EXWST
C       Modified 8/1990 by CJC for ROM 2.2 -- TEMP.LET; exits on CONTROL-Z.
C       Modified 2/1993 by CJC for CRAY
C       Modified 8/1996 by CJC to treat "!" as a terminator
C       Modified 1/1997 by CJC:  logs result
C       Revised  5/2003 by CJC:  factor through M3MSG2 to ensure flush()
C       of log-messages.
C       Revised 6/2003 by CJC:  factor through M3PROMPT to ensure flush()
C       of PROMPT for IRIX F90v7.4  
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO.
C       Fix MH violation of coding-standards:  check status IOS from  ENVYN!!
C***********************************************************************

        IMPLICIT NONE

C.......   ARGUMENTS:

        INTEGER      , INTENT(IN   ) :: ITEMCNT         !  number of choices
        INTEGER      , INTENT(IN   ) :: DEFAULT         !  default response
        CHARACTER*(*), INTENT(IN   ) :: PROMPT          !  prompt string
        CHARACTER*(*), INTENT(IN   ) :: CHOICES ( * )   !  array of choice strings

C.......   EXTERNAL FUNCTION:

        LOGICAL, EXTERNAL :: ENVYN

C.......   LOCAL VARIABLES:

        INTEGER         LENGTH          !  length of default as ASCII string
        INTEGER         I , M
        INTEGER         ANSWER
        INTEGER         ERRCNT
        INTEGER         IOS
        CHARACTER*32    BUFFER
        CHARACTER*8     FMTSTR
        CHARACTER*256   MESG

        LOGICAL, SAVE :: PROMPTON
        LOGICAL, SAVE :: FIRSTIME = .TRUE.

        CHARACTER*16, PARAMETER :: PNAME = 'GETMENU'

C...............................................................
C   begin body of GETMENU

        IF( FIRSTIME ) THEN

            PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',
     &                      .TRUE., IOS )
            IF ( IOS .GT. 0 ) THEN
                CALL M3EXIT( PNAME,0,0,'Bad env vble "PROMPTFLAG"', 2 )
            END IF
            FIRSTIME = .FALSE.
 
        END IF

        IF( .NOT. PROMPTON ) THEN
            GETMENU = DEFAULT
            MESG = 'Using default response "' //
     &          TRIM( CHOICES( DEFAULT ) ) // '" for query:'
            CALL M3MSG2( MESG )
            MESG = '"' // TRIM( PROMPT ) // '"'
            CALL M3MSG2( MESG )
            RETURN
        END IF

        IF ( ITEMCNT .GT. 9999 ) THEN
            WRITE ( 6,91010 )
     &              'Maximum number of items:  ', 9999,
     &              'Number of requested items:', ITEMCNT
            CALL M3EXIT( 'GETMENU', 0, 0, 
     &                   'Maximum item count exceeded.', 1 )
        ELSE IF ( ITEMCNT .LT. 1 ) THEN
            WRITE ( 6,91010 )
     &              'Minimum number of items:  ', 1,
     &              'Number of requested items:', ITEMCNT
            CALL M3EXIT( 'GETMENU', 0, 0,
     &                   'Minimum item count exceeded.', 1 )
        END IF

        I       =  1
        LENGTH  =  0
10      CONTINUE

            I       =  I * 10
            LENGTH  =  LENGTH  +  1
            IF ( I .LE. DEFAULT )  GO TO  10

        ERRCNT =  0

100     CONTINUE

        WRITE ( 6,* )
        WRITE ( 6,92010 )
        IF ( ITEMCNT .LE. 18 )  WRITE ( 6,92020 )

        DO  200  I = 1 , ITEMCNT

            WRITE ( 6,92030 ) I , TRIM( CHOICES ( I ) )

200     CONTINUE

        IF ( ITEMCNT .LE. 18 )  WRITE ( 6,92020 )
        WRITE ( 6,92010 )

        IF ( ITEMCNT .LT. 10 )THEN      !  select prompt-format:
            BUFFER = '( 2A, I1, A )'
        ELSE IF ( ITEMCNT .LT. 100 )THEN
            BUFFER = '( 2A, I2, A )'
        ELSE
            BUFFER = '( 2A, I4, A )'
        END IF

        WRITE( MESG, BUFFER ) TRIM( PROMPT ), ' [', DEFAULT, '] >> '
        CALL M3PROMPT( MESG, BUFFER, IOS )

        IF ( IOS .NE. 0 )  THEN
            GO TO 900
        ELSE IF ( BUFFER ( 1:1 )  .EQ. ' ' )  THEN
            GETMENU  =  DEFAULT
            MESG = 'Using default "' // TRIM( CHOICES( DEFAULT ) )//'"'
        ELSE
            WRITE( FMTSTR, 94010 ) LEN_TRIM( BUFFER )

            READ( BUFFER, FMTSTR, IOSTAT=IOS, ERR=400 ) ANSWER

            IF ( ANSWER .LT. 1  .OR.  ANSWER .GT. ITEMCNT )  THEN

                ERRCNT  =  ERRCNT + 1
                WRITE ( 6,91020 ) ANSWER , ITEMCNT , ERRCNT
                CALL M3FLUSH( 6 )
                IF ( ERRCNT .LT. 5 )  GO TO  100

                GO TO  500      !  max error count exceeded

            END IF

            GETMENU  =  ANSWER
            MESG = 'Using response "'// TRIM( CHOICES( ANSWER ) )// '"'
        END IF
        CALL M3MSG2( MESG )

        RETURN


400     CONTINUE        !  error in read from BUFFER

        ERRCNT  =  ERRCNT + 1
        WRITE ( 6,91030 )  ERRCNT
        CALL M3FLUSH( 6 )
        IF ( ERRCNT .LT. 5 )  GO TO  100


500     CONTINUE        !  more than 5 entry errors

        CALL M3EXIT( 'GETMENU', 0, 0, 
     &               'Maximum error count exceeded', 2 )


900     CONTINUE        !  error in read from terminal

        ERRCNT  =  ERRCNT + 1
        WRITE ( 6,91030 )  ERRCNT
        CALL M3FLUSH( 6 )
        IF ( ERRCNT .LT. 5 )  GO TO  100
        WRITE ( 6,91040 ) IOS
        CALL M3EXIT( 'GETMENU', 0, 0, 'MAX ERRCNT exceeded', 1 )


C.................   end body of GETMENU  ..............................

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( 2 ( /10X , A, I12 ) )

91020   FORMAT ( /5X , '>>>  RESPONSE ERROR  <<<'      ,
     &         //10X , 'Your response'          , I6 , 2X,
     &                 'not in the range 1 ...' , I6 ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'            , / )

91030   FORMAT ( /5X , '>>>  RESPONSE ERROR  <<<'         ,
     &         //10X , 'Did not understand your response' ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'                , / )

91040   FORMAT ( //10X  , 'Error number = ' , I3 )


C...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( 1X , 79('*') )

92020   FORMAT ( 1X , '*' , T79 , '*' )

92030   FORMAT ( 1X , '*  ' , I2 , ':  ' , A , T79 , '*' )


C...........   Internal buffering formats............ 94xxx

94010   FORMAT( '( I', I3, ')' )


        END FUNCTION GETMENU

