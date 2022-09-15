
        REAL   FUNCTION GETREAL( LO , HI , DEFAULT , PROMPT )

C********************************************************************
C Version "$Id: getreal.f 86 2018-03-13 18:52:39Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (c) 2004-2007 Baron Advanced Meteorological Systems,
C (c) 2007-2013 Carlie J. Coats, Jr., and (C) 2014 UNC Institute
C for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C       function getreal()   body starts at line  92
C       entry    getreal1()  body starts at line 189
C
C   FUNCTION:
C
C       Display the  PROMPT  for an integer between  LO  and  HI,
C       get the user's response and check that it is within range.
C       Return DEFAULT if the user hits <RET>.  Reprompts on error
C       for up to 5 attempts.
C
C  REVISION HISTORY:
C
C       Adapted  9/1990 by CJC from GETNUM
C       Version  2/1993 by CJC for CRAY
C       Version  8/1996 by CJC treats ! as a terminator
C       Modified 1/1997 by CJC:  logs result
C       Modified 8/1998 by CJC:  PROMPT2 and strict f77 string-handling
C       Revised  5/2003 by CJC:  log result, factored through M3MSG2
C                to ensure flush() of log messages
C       Revised   6/2003 by CJC:  factor through M3MSG2, M3PROMPT, and
C                 M3FLUSH to ensure flush() of PROMPT and of log-messages
C                 for IRIX F90v7.4  
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C       Modified 02/2014 by CJC: ENTRY GETREAL1() does not have bounds LO, HI;
C                 use F90 "READ( BUFFER,*,...)"
C       Modified 02/2015 by CJC for I/O API 3.2: Fix MH violation of 
C       coding-standards:  check status IOS from  ENVYN() !!
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C
C        LO       Minimum allowed return value
C        HI       Maximum allowed return value
C        DEFAULT  Default return value
C        PROMPT   Prompt for user
C
C    Output arguments:  none
C
C  RETURNS   user response after checking its range; or default.
C
C********************************************************************

        IMPLICIT NONE
        INCLUDE 'PARMS3.EXT'

C.......   ARGUMENTS:

        REAL         , INTENT(IN   ) :: LO, HI, DEFAULT
        CHARACTER*(*), INTENT(IN   ) :: PROMPT

        REAL            GETREAL1

C.......   EXTERNAL FUNCTION:  interpret I/O errors:

        LOGICAL, EXTERNAL :: ENVYN

        CHARACTER*16, PARAMETER :: PNAME   = 'GETREAL'


C.......   LOCAL VARIABLES:

        INTEGER         MODE                !!  mode=1 for getnum(), mode=0 for getnum1()
        INTEGER         K , L , M , N
        REAL            LLO , LHI , LDF
        REAL            ANSWER
        INTEGER         ERRCNT
        INTEGER         IOS
        CHARACTER*64    BUFFER
        CHARACTER*256   MESG
        CHARACTER*16	FMTSTR
        CHARACTER*1	CH

        LOGICAL, SAVE :: PROMPTON
        LOGICAL, SAVE :: FIRSTIME = .TRUE.

C*********************************************************************
C       begin GETREAL

        MODE = 1

        LLO  =  MIN ( LO , HI )
        LHI  =  MAX ( LO , HI )
        LDF  =  MIN ( LHI , MAX ( LLO , DEFAULT ) )

11      CONTINUE        !!  target of entry getreal1()

        IF( FIRSTIME ) THEN

            PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',
     &                        .TRUE., IOS )
            IF ( IOS .GT. 0 ) THEN
                CALL M3EXIT( PNAME,0,0,'Bad env vble "PROMPTFLAG"', 2 )
            END IF
            FIRSTIME = .FALSE.
 
        END IF

        IF( .NOT. PROMPTON ) THEN
            GETREAL = DEFAULT
            WRITE( MESG,94030 ) DEFAULT, TRIM( PROMPT )
            CALL M3MSG2( MESG )
            RETURN
        END IF

        ERRCNT =  0


100     CONTINUE

        WRITE( MESG, '( 2A , 1PE12.5, A )' )
     &        TRIM( PROMPT ), ' [', LDF, '] >> '
        CALL M3PROMPT( MESG, BUFFER, IOS )

        IF ( IOS .NE. 0 ) GO TO 900

        IF ( BUFFER .EQ. ' ' )  THEN
            GETREAL =  LDF
            WRITE( MESG,94020 ) TRIM( PROMPT ), LDF
            CALL M3MSG2( MESG )
        ELSE

            READ( BUFFER, *, IOSTAT=IOS ) ANSWER

            IF ( IOS .NE. 0 ) GO TO 900

            IF ( MODE .EQ. 0 ) THEN

                CONTINUE        !!  getnum1() -- don't do range-check

            ELSE IF ( ANSWER .LT. LLO  .OR.  ANSWER .GT. LHI )  THEN
                ERRCNT  =  ERRCNT + 1
                WRITE ( 6,92100 ) ANSWER , LLO , LHI , ERRCNT
                IF ( ERRCNT .LT. 5 )  GO TO  100

                GO TO  500      !  max error count exceeded

            END IF

            GETREAL =  ANSWER
            WRITE( MESG,94020 ) TRIM( PROMPT ), ANSWER
            CALL M3MSG2( MESG )

        END IF

        RETURN


400     CONTINUE        !  error in read from BUFFER

        ERRCNT  =  ERRCNT + 1
        WRITE ( 6,92200 )  ERRCNT
        CALL M3FLUSH( 6 )
        IF ( ERRCNT .LT. 5 )  GO TO  100


500     CONTINUE        !  more than 5 entry errors

        CALL M3EXIT( 'GETREAL',0,0, 
     &               'Maximum entry-error count exceeded', 2 )


900     CONTINUE        !  error in read from terminal

        ERRCNT  =  ERRCNT + 1
        IF ( ERRCNT .LT. 5 ) THEN
            WRITE ( 6,92000 ) IOS , ERRCNT
            CALL M3FLUSH( 6 )
            GO TO  100
        ELSE
            CALL M3EXIT( 'GETREAL',0,0, 
     &                   'Maximum error count exceeded', 1 )
        END IF

C................   end body of GETREAL  .......................................

        ENTRY GETREAL1( DEFAULT , PROMPT )   !!  no "lo" nor "hi" bounds for result
        
        MODE = 0
        LLO  =  BADVAL3
        LHI  = -BADVAL3
        LDF  =  MIN ( LHI , MAX ( LLO , DEFAULT ) )
        GO TO 11

C................   end body of GETREAL1  .......................................


92000   FORMAT ( /5X , '>>> ERROR IN ROUTINE GETREAL <<< ' ,
     &         //10X , 'Error reading response'               ,
     &         /10X  , 'I/O error status number = ' , I3                 ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'
     &         )

92100   FORMAT ( /5X , '>>>  RESPONSE ERROR  <<<'      ,
     &         //10X , 'Your response '           , 1PE12.5 ,
     &                 ' not in the range '       , 1PE12.5 ,
     &                 ' ... ' , 1PE12.5 ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'            , /
     &         )

92200   FORMAT ( /5X , '>>>  RESPONSE ERROR  <<<'         ,
     &         //10X , 'Did not understand your response' ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'                , /
     &         )

94010	FORMAT ( '(I', I3, ')' )

94011	FORMAT ( '(', A1, I3, '.', I3 , ')' )

94020	FORMAT ( 10 ( A, :, 1PE12.5 , :, 2X ) )

94030	FORMAT ( 'Using default response', 1PE12.5 , 2X, 
     &           'for query "', A, '"' )

        END FUNCTION GETREAL

