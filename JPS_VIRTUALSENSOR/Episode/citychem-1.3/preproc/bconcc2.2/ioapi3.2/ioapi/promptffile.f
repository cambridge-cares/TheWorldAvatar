 
        INTEGER FUNCTION PROMPTFFILE( PROMPT, 
     &                                RDONLY, FMTTED, DEFAULT, CALLER )

C***********************************************************************
C Version "$Id: promptffile.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line 88
C
C       Prompts user for logical file name, then opens the Fortran file
C       associated with it, for read-only or not, and formatted or not,
C       as indicated by RDONLY and FMTTED
C
C  RETURNS:
C       unit number for the file opened, or 
C       -1 for failure, 
C       -2 for 'NONE', provided that '"NONE"' occurs within the prompt; or
C       -3 for 'ALL',  provided that '"ALL"'  occurs within the prompt
C
C  PRECONDITIONS REQUIRED:
C       "setenv <lname> <pathname>" for the file before program launch
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETEFILE
C
C  REVISION  HISTORY:
C       prototype 6/95 by CJC
C       Revised  10/95 by CJC:  more robust treatment of 'NONE'
C       Modified  8/96 by CJC:  ! is a comment-designator for input
C       Modified  4/99 by CJC & M. Houyoux:  turn on/off prompting with
C       environment variable "PROMPTFLAG"
C       Revised   6/2003 by CJC:  factor through M3MSG2, M3PROMPT, and
C       M3FLUSH to ensure flush() of PROMPT and of log-messages for
C       IRIX F90v7.4  
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C       Modified 02/2015 by CJC for I/O API 3.2:  Fix MH violation
C       of coding-standards:  check status IOS from  ENVYN!!
C***********************************************************************

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        
        CHARACTER*(*), INTENT(IN   ) :: PROMPT         !  prompt for user
        LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
        LOGICAL      , INTENT(IN   ) :: FMTTED         !  TRUE iff file should be formatted
        CHARACTER*(*), INTENT(IN   ) :: DEFAULT        !  default logical file name
        CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging messages

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: GETEFILE
        LOGICAL, EXTERNAL :: ENVYN, GETYN

C...........   PARAMETERS:

        CHARACTER*16, PARAMETER :: PNAME   = 'PROMPTFFILE'
        CHARACTER*16, PARAMETER :: BLANK16 = ' '
        CHARACTER*16, PARAMETER :: NONE16  = 'NONE'
        CHARACTER*16, PARAMETER :: ALL16   = 'ALL'

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*16    LNAME        !  logical file name
        INTEGER         I            !  position at which "!" found
        INTEGER         IOS          !  I/O error status
        INTEGER         IDEV         !  unit number
        INTEGER         PLEN, DLEN   !  trimlen( prompt | default )
        CHARACTER*512   BUF          !  prompt/message buffer
        CHARACTER*256   MESG         !  messages
        LOGICAL         AFLAG        !  "ALL"  is in the prompt
        LOGICAL         NFLAG        !  "NONE" is in the prompt

        LOGICAL, SAVE :: PROMPTON     !  Actually prompt or open default
        LOGICAL, SAVE :: FIRSTIME = .TRUE.


C***********************************************************************
C   begin body of function  PROMPTFFILE

        IF( FIRSTIME ) THEN

            PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',
     &                        .TRUE., IOS )
            IF ( IOS .GT. 0 ) THEN
                CALL M3EXIT( PNAME,0,0,'Bad env vble "PROMPTFLAG"', 2 )
            END IF
            FIRSTIME = .FALSE.

        ENDIF

C.......   Get file name; open input control definition file

        AFLAG = ( INDEX( PROMPT, '"ALL"'  ) .GT. 0 )
        NFLAG = ( INDEX( PROMPT, '"NONE"' ) .GT. 0 )

        PLEN  = LEN_TRIM( PROMPT  )
        DLEN  = LEN_TRIM( DEFAULT )

        IF ( DLEN .GT. 16 ) THEN
            WRITE( MESG, '( A, A, A, I6, 2X, A )' )
     &      'Length of DEFAULT "',  DEFAULT( 1:DLEN ) , 
     &      '" exceeds 16; truncating'
            BUF = TRIM( CALLER ) // '/PROMPTFFILE'
            CALL M3WARN( BUF, 0, 0, MESG )
            DLEN = 16
        END IF

        IF( PROMPTON ) THEN

            IF ( DLEN + PLEN .GT. 250 ) THEN
                WRITE( MESG, '( A, A, A, I6, 2X, A )' )
     &          'Prompt too long; truncating'
                BUF = TRIM( CALLER ) // '/PROMPTFFILE'
                CALL M3WARN( BUF, 0, 0, MESG )
                PLEN = 250 - DLEN
            END IF

            BUF = TRIM( PROMPT  ) // ' [' //
     &            TRIM( DEFAULT ) // '] >> '

11          CONTINUE
        
                CALL M3PROMPT( BUF, LNAME, IOS )

                IF ( IOS .NE. 0 ) THEN

                    MESG = 'Could not read your response'
                    CALL M3MSG2( MESG )
                    IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                        GO TO  11
                    ELSE
                        MESG = 'Could not read logical name for file'
                        CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
                    END IF

                END IF      !  if could not read response

                I = INDEX( LNAME, '!' )
                IF ( I .GT. 0 ) LNAME( I : LEN( LNAME ) ) = ' '

                IF ( LNAME .EQ. BLANK16 )  THEN
                    LNAME = DEFAULT
                END IF

                IF ( AFLAG .AND. ( LNAME .EQ. ALL16 ) )  THEN
                    PROMPTFFILE = -3
                    RETURN
                ELSE IF ( NFLAG .AND. LNAME .EQ. NONE16 )  THEN
                    PROMPTFFILE = -2
                    RETURN
                END IF
    
                IDEV = GETEFILE( LNAME, RDONLY, FMTTED, CALLER )
                IF ( IDEV .LT. 0 ) THEN     !  failure to open

                    MESG = 'Could not open input file "' //
     &                     TRIM( LNAME ) // '".'
                    CALL M3MSG2( MESG )
                    IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                        GO TO  11
                    ELSE
                        MESG = 'Ending program "' //
     &                          TRIM( CALLER ) // '".'
                        CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
                    END IF

                END IF      !  if getefile() failed

        ELSE   ! Do not prompt for output

            LNAME = DEFAULT 

            IF ( AFLAG .AND. ( LNAME .EQ. ALL16 ) )  THEN
                 PROMPTFFILE = -3
                 RETURN

            ELSE IF ( NFLAG )  THEN

                IF ( LNAME .EQ. NONE16 )  THEN
                    PROMPTFFILE = -2
                    RETURN
                END IF

C           ..  Check if logical name is set in order to permit
C           ..  Study Planner to skip file without having to input "NONE"

                CALL ENVSTR( LNAME, 'Input file name', BLANK16,
     &                       BUF, IOS )

                IF( IOS .LT. 0 ) THEN
                    PROMPTFFILE = -2
                    RETURN
                END IF

            END IF

            IDEV = GETEFILE( LNAME, RDONLY, FMTTED, CALLER )
            IF ( IDEV .LT. 0 ) THEN     !  failure to open
                MESG = 'Could not open input file "' //
     &                  TRIM( LNAME ) // '".'
                CALL M3MSG2( MESG )
                MESG = 'Ending program "' //
     &                  TRIM( CALLER ) // '".'
                CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
            END IF

        END IF

        PROMPTFFILE = IDEV
        RETURN

        END FUNCTION PROMPTFFILE

