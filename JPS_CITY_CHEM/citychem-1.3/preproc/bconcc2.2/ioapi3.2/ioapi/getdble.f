
        REAL*8 FUNCTION GETDBLE( LO, HI, DEFAULT, PROMPT )

C********************************************************************
C Version "$Id: getdble.f 86 2018-03-13 18:52:39Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (c) 2004-2007 Baron Advanced Meteorological Systems,
C (c) 2007-2013 Carlie J. Coats, Jr., and (C) 2014 UNC Institute
C for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C       function getdble()   body starts at line  83
C       entry    getdble1()  body starts at line 183
C
C  FUNCTION:
C
C       Display the  PROMPT  for an integer between  LO  and  HI,
C       get the user's response and check that it is within range.
C       Return DEFAULT if the user hits <RET>.  Reprompts on error
C       for up to 5 attempts.
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
C  CALLS:
C
C  RETURNS   user response after checking its range; or default.
C
C  REVISION  HISTORY:
C       prototype 4/2003 by Carlie J. Coats, Jr, BAMS, adapted fromn GETREAL
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C       Modified 02/2014 by CJC: ENTRY GETDBLE1() does not have bounds LO, HI;
C       use F90 "READ( BUFFER,*,...)"; Fix MH violation of coding-standards:
C       check status IOS from  ENVYN() !!
C********************************************************************

        IMPLICIT NONE

C.......   ARGUMENTS:

        REAL*8       , INTENT(IN   ) :: LO , HI
        REAL*8       , INTENT(IN   ) :: DEFAULT
        CHARACTER*(*), INTENT(IN   ) :: PROMPT

        REAL*8          GETDBLE1

C.......   EXTERNAL FUNCTION:  interpret I/O errors:

        LOGICAL, EXTERNAL :: ENVYN


C.......   LOCAL VARIABLES:

        INTEGER         MODE                !!  mode=1 for getdble(), mode=0 for getdble1()
        INTEGER         K , L , M , N, P
        REAL*8          LLO , LHI , LDF
        REAL*8          ANSWER
        INTEGER         ERRCNT
        INTEGER         IOS
        CHARACTER*64    BUFFER
        CHARACTER*16    FMTSTR
        CHARACTER*1     CH
        CHARACTER*256   MESG

        LOGICAL, SAVE :: PROMPTON
        LOGICAL, SAVE :: FIRSTIME = .TRUE.

        REAL*8      , PARAMETER :: BADDBL = -9.999D306
        CHARACTER*16, PARAMETER :: PNAME  = 'GETDBLE'

C*********************************************************************
C       begin GETDBLE

        MODE = 1
        LLO  =  DMIN1( LO , HI )
        LHI  =  DMAX1( LO , HI )
        LDF  =  DMIN1( LHI , DMAX1( LLO , DEFAULT ) )

11      CONTINUE        !!  target of entry getdble1()

        IF( FIRSTIME ) THEN

            PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',
     &                      .TRUE., IOS )
            IF ( IOS .GT. 0 ) THEN
                CALL M3EXIT( PNAME,0,0,'Bad env vble "PROMPTFLAG"', 2 )
            END IF
            FIRSTIME = .FALSE.
 
        END IF

        IF( .NOT. PROMPTON ) THEN
            GETDBLE = DEFAULT
            WRITE( MESG, '( A , 1X , 1PD22.15, 1X, A )' ) 
     &          'Using default response', DEFAULT, ' for query:'
            CALL M3MSG2( MESG )
            MESG = '"' // TRIM( PROMPT ) // '"'
            CALL M3MSG2( MESG )
            RETURN
        END IF

        ERRCNT =  0


100     CONTINUE

        WRITE( MESG, '( 2A , 1PD22.15, A )' )
     &        TRIM( PROMPT ), ' [', LDF, '] >> '
        CALL M3PROMPT( MESG, BUFFER, IOS )

        IF ( IOS .NE. 0 )  THEN
            GO TO  900
        ELSE IF ( BUFFER ( 1:1 )  .EQ. ' ' )  THEN
            GETDBLE =  LDF
            WRITE( MESG, '( A , 1X , 1PD22.15 )' ) 'Using default', LDF
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

            GETDBLE  =  ANSWER
            
            WRITE( MESG, '( A,1X,1PD22.15 )' ) 'Using response', ANSWER

        END IF

        CALL M3MSG2( MESG )

        RETURN


400     CONTINUE        !  error in read from BUFFER

        ERRCNT  =  ERRCNT + 1
        WRITE ( 6,92200 )  ERRCNT
        CALL M3FLUSH( 6 )
        IF ( ERRCNT .LT. 5 )  GO TO  100


500     CONTINUE        !  more than 5 entry errors

        CALL M3EXIT( 'GETDBLE',0,0, 'Maximum error count exceeded', 2 )


900     CONTINUE        !  error in read from terminal

        ERRCNT  =  ERRCNT + 1
        IF ( ERRCNT .LT. 5 ) THEN
            WRITE ( 6,92000 ) IOS , ERRCNT
            CALL M3FLUSH( 6 )
            GO TO  100
        ELSE
            CALL M3EXIT( 'GETDBLE', 0, 0, 
     &                   'Maximum error count exceeded', 1 )
        END IF

C................   end body of GETDBLE  .......................................

        ENTRY GETDBLE1( DEFAULT , PROMPT )   !!  no "lo" nor "hi" bounds for result
        
        MODE = 0
        LLO  =  DBLE(  BADDBL )
        LHI  =  DBLE( -BADDBL )
        LDF  =  MIN( LHI , MAX( LLO , DEFAULT ) )
        GO TO 11

C................   end body of GETDBLE1  .......................................


92000   FORMAT ( /5X , '>>> ERROR IN ROUTINE GETDBLE <<< ' ,
     &         //10X , 'Error reading response'               ,
     &         /10X  , 'I/O error status number = ' , I3                 ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'
     &         )

92100   FORMAT ( /5X , '>>>  RESPONSE ERROR  <<<'      ,
     &         //10X , 'Your response '     , 1PD22.15 ,
     &                 ' not in the range ' , 1PD22.15 ,
     &                 ' ... ' , 1PD22.15 ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'            , /
     &         )

92200   FORMAT ( /5X , '>>>  RESPONSE ERROR  <<<'         ,
     &         //10X , 'Did not understand your response' ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'                , /
     &         )

94010   FORMAT ( '(I', I3, ')' )

94011   FORMAT ( '(', A1, I3, '.', I3 , ')' )

        END FUNCTION GETDBLE
