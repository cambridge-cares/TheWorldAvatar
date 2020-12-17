
        CHARACTER*16 FUNCTION PROMPTMFILE( PROMPT, FMODE, 
     &                                     DEFAULT, CALLER )

C***********************************************************************
C Version "$Id: promptmfile.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line 94
C
C       If environment variable PROMPTFLAG is 'Y', returns DEFAULT.
C
C       Prompts user for logical file name, then opens the I/O API file
C       associated with it, using the indicated file mode (FSREAD3, 
C       FSRDWR3, FSNEW3, FSUNKN3)
C
C       Provided that '"NONE"'occurs within the prompt, if name entered 
C       is 'NONE', does not attempt to open the file (but still returns 
C       'NONE' as the function value).
C
C  RETURNS:
C       logical name of file opened
C
C  PRECONDITIONS REQUIRED:
C       "setenv <lname> <pathname>" for the file before program launch
C       file description set in FDESC3.EXT structures if appropriate
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETYN, OPEN3
C
C  REVISION  HISTORY:
C       prototype 6/1995 by CJC
C	Revised  10/1995 by CJC:  more robust treatment of 'NONE'
C       Modified  8/1996 by CJC:  ! is a comment-designator for input
C       Modified  8/1997 by MH:   environment variable PROMPTFLAG
C       Revised   6/2003 by CJC:  factor through M3MSG2, M3PROMPT, and
C       M3FLUSH to ensure flush() of PROMPT and of log-messages for
C       IRIX F90v7.4  
C       Revised   7/2003 by CJC:  clean up LUNIT=INIT3() and
C       FIRST-TIME logic
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C       Modified 02/2015 by CJC for I/O API 3.2:   Fix MH violation of
C       coding-standards:  check status IOS from  ENVYN!!
C***********************************************************************

      IMPLICIT NONE

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'IODECL3.EXT'

C...........   ARGUMENTS and their descriptions:
        
        CHARACTER*(*), INTENT(IN   ) :: PROMPT         !  prompt for user
        INTEGER      , INTENT(IN   ) :: FMODE          !  file opening-mode
        CHARACTER*(*), INTENT(IN   ) :: DEFAULT        !  default logical file name
        CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging messages


C...........   PARAMETERS:

        CHARACTER*16, PARAMETER :: PNAME   = 'PROMPTMFILE'
        CHARACTER*16, PARAMETER :: BLANK16 = ' '
        CHARACTER*16, PARAMETER :: NONE16  = 'NONE'

C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: ENVYN, GETYN

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         I            !  position at which "!" found
        CHARACTER*16    LNAME        !  logical file name
        INTEGER         IOS          !  I/O status
        CHARACTER*120   MESG         !  messages
        CHARACTER*512   BUFFER       !  prompt/environment buffer
        LOGICAL         NFLAG        !  "NONE" is in the prompt

 
C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER, SAVE :: LUNIT = IMISS3
        LOGICAL, SAVE :: PROMPTON     !  Actually prompt or open default

 
C***********************************************************************
C   begin body of function  PROMPTMFILE

        IF( LUNIT .LT. 0 ) THEN
 
            LUNIT = INIT3()

            CALL M3MSG2( BLANK16 )
            PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',
     &                        .TRUE., IOS )
            IF ( IOS .GT. 0 ) THEN
                CALL M3EXIT( PNAME,0,0,'Bad env vble "PROMPTFLAG"', 2 )
            END IF

         END IF         !  if firstime:  lunit < 0


C.......   Decide whether 'NONE' is a valid response

        NFLAG = ( INDEX( PROMPT, '"NONE"' ) .GT. 0 )

        IF( PROMPTON ) THEN

C.......   Construct actual prompt; Loop:  get file name until file opens
        
            BUFFER = TRIM( PROMPT  ) // ' [' //
     &               TRIM( DEFAULT ) // '] >> '

11          CONTINUE
        
                LNAME = ' '
                CALL M3PROMPT( BUFFER, LNAME, IOS )
            
                IF ( IOS .NE. 0 ) THEN

                    MESG = 'Could not read your response'
                    CALL M3MSG2( MESG )
                    IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                        GO TO  11
                    ELSE
                        MESG = 'Could not read logical name for file'
                        CALL M3EXIT( 'CTLAMAT', 0, 0, MESG, 2 )
                    END IF

                END IF      !  if could not read response

                I = INDEX( LNAME, '!' )
                IF ( I .GT. 0 ) LNAME( I : LEN( LNAME ) ) = ' '

                IF ( LNAME .EQ. BLANK16 ) THEN
                    LNAME = DEFAULT
                END IF

                IF ( NFLAG .AND. ( LNAME .EQ. NONE16 ) ) THEN
                    PROMPTMFILE = NONE16
                    RETURN
                END IF

                IF ( .NOT. OPEN3( LNAME, FMODE, CALLER ) ) THEN !  failure to open

                    MESG = 'Could not open file "' //
     &                     TRIM( LNAME ) // '".'
                    CALL M3MSG2( MESG )
                    IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                        GO TO  11
                    ELSE
                        MESG = 'Ending program "' //
     &                          TRIM( CALLER ) // '".'
                        CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
                    END IF

                END IF      !  if open3() failed

        ELSE  ! Do not prompt for output

            LNAME = DEFAULT

            IF ( NFLAG )  THEN
 
                IF( LNAME .EQ. NONE16      ) THEN
                    PROMPTMFILE = NONE16
                    RETURN
                END IF

C           ..  Check if logical name is set in order to permit
C           ..  Study Planner to skip file without having to input "NONE"
 
                CALL ENVSTR( LNAME, 'Input file name', ' ',
     &                       BUFFER, IOS )
 
                IF( IOS .LT. 0 ) THEN   ! either not set (-2) or empty (-1)
                    PROMPTMFILE = NONE16
                    RETURN
                END IF

            END IF              !  if nflag (checking for "none"

            IF ( .NOT. OPEN3( LNAME, FMODE, CALLER ) ) THEN !  failure to open
 
                MESG = 'Could not open file "' // TRIM( LNAME ) // '".'
                CALL M3MSG2( MESG )
                CALL M3EXIT( CALLER, 0, 0, MESG, 2 )

            END IF      !  if open3() failed

        ENDIF

        PROMPTMFILE = LNAME
        RETURN

        END FUNCTION PROMPTMFILE

