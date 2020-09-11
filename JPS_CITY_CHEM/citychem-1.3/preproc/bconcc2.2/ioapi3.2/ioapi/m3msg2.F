
        SUBROUTINE M3MSG2( MESSAGE )

C***********************************************************************
C Version "$Id: m3msg2.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010  Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  Subroutine  M3MSG2   starts at line   96
C  Subroutine  M3MESG   starts at line  139
C  Subroutine  M3PROMPT starts at line  196
C  Subroutine  M3PARAG  starts at line  261
C  Subroutine  M3FLUSH  starts at line  324
C  subroutine  FLUSH    starts at line  348
C
C  FUNCTION:
C       M3MSG2:    Generate simple (1-line text) messages. 
C       M3PROMPT:  Generate prompt-and-response;
C       M3PARAG:   Generate multi-line text messages;
C       M3FLUSH:   wrapper around FLUSH() -- needed for IBM XLF.
C       FLUSH:     overide system FLUSH when IOAPI_NOFLUSH
C
C  PRECONDITIONS REQUIRED:  
C       message fits on one line
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       INIT3, TRIMLEN
C
C  REVISION  HISTORY:   
C       Adapted   1/1997 by CJC from M3MESG()
C
C       Modified  5/1998 by CJC for OpenMP thread-safety
C
C       Modified 10/1999 by CJC for AIX "flush"
C
C       Modified  5/2003 by CJC for control of optional messages via
C       STATE3 variable STDOUT; changes to critical sections.
C
C       Modified  6/2003 by CJC to deal with IRIX F90v7.4.x failure to
C       properly ANSWER and flush output on program exit:  new entries
C       M3PROMPT used by the GET*() and PROMPT*() routines, and 
C       M3FLUSH for use by user-code.
C
C       Modified  9/2003 by CJC: new entry "M3PARAG"
C
C       Modified 12/2003 by CJC:  Hack "CALL INITBLK3" to deal with 
C       failure of SGI version 7.4 compilers to follow industry standards;
C       re-structuring with INITLOG3() to avoid problems with nested
C       critical sections.
C
C       Modified 4/2004 by CJC:  absorbtion of M3MESG() into  M3MSG2()
C       as an overlapping ENTRY.
C
C       Modified 12/2007 by CJC:  mollify Intel "ifort" by turning this
C       back into a set of subroutines. 
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1 
C***********************************************************************

      IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: MESSAGE

                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INITLOG3
        EXTERNAL          :: INITBLK3        !!  block data to initialize STATE3 commons


C.......   Scratch LOCAL VARIABLES

        INTEGER         L, M
        LOGICAL         STDOUTFLAG


C......................................................................
C   begin body of subroutine  M3MSG2

        STDOUTFLAG = .FALSE.
#ifdef IOAPI_STDOUT
        STDOUTFLAG = .TRUE.
#endif

11      CONTINUE        !  target of "GO TO" in entry M3MESG()

        L = LEN_TRIM( MESSAGE )

!$OMP CRITICAL( S_LOGDEV )

#if defined(__sgi)    || defined(__mips__)

            !!==========>  *HACK*  to deal with SGI v7.4 or later compilers,
            !!             that do not follow industry standards with respect
            !!             to how to make sure that BLOCK DATA modules are
            !!             properly linked into an executable:

            CALL INITBLK3

#endif

            IF ( LOGDEV .LT. 0 ) THEN
                LOGDEV = INITLOG3( 'M3MSG2' )
            END IF          !!  if logdev < 0 (initialization required)

            WRITE( LOGDEV,'( 5X, A )' ) MESSAGE( 1:L )
            CALL FLUSH( LOGDEV )

            IF ( STDOUTFLAG .AND. LOGDEV .NE. 6 ) THEN
                WRITE( 6,'( 5X, A )' ) MESSAGE( 1:L )
                CALL FLUSH( 6 )
            END IF          !  if logdev is not pointing at standard output

!$OMP END CRITICAL( S_LOGDEV )

        RETURN
        END  SUBROUTINE M3MSG2


C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C   begin subroutine  M3MESG

        SUBROUTINE M3MESG( MESSAGE )

        IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: MESSAGE

                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INITLOG3
        EXTERNAL          :: INITBLK3        !!  block data to initialize STATE3 commons

C......................................................................
C   begin body of subroutine  M3MESG

!$OMP CRITICAL( S_LOGDEV )

#if defined(__sgi)    || defined(__mips__)

            !!==========>  *HACK*  to deal with SGI v7.4 or later compilers,
            !!             that do not follow industry standards with respect
            !!             to how to make sure that BLOCK DATA modules are
            !!             properly linked into an executable:

            CALL INITBLK3

#endif

            IF ( LOGDEV .LT. 0 ) THEN
                LOGDEV = INITLOG3( 'M3MSG2/M3MESG' )
            END IF          !!  if logdev < 0 (initialization required)

            WRITE( LOGDEV,'( 5X, A )' ) TRIM( MESSAGE )
            CALL FLUSH( LOGDEV )

!$OMP END CRITICAL( S_LOGDEV )

        RETURN
        END SUBROUTINE M3MESG


C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C   begin subroutine  M3PROMPT

        SUBROUTINE M3PROMPT( MESSAGE, ANSWER, ISTAT )

        IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: MESSAGE
        CHARACTER*(*), INTENT(  OUT) :: ANSWER
        INTEGER      , INTENT(  OUT) :: ISTAT
                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INITLOG3
        EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons


C.......   Scratch LOCAL VARIABLES

        INTEGER     L, M


C......................................................................
C   begin body of subroutine  M3PROMPT

        L = LEN_TRIM( MESSAGE )

!$OMP   CRITICAL( S_LOGDEV )

            IF ( LOGDEV .LT. 0 ) THEN
                LOGDEV = INITLOG3( 'M3MSG2/M3PROMPT' )
            END IF          !!  if logdev < 0 (initialization required)

            WRITE( LOGDEV,'( 5X, A, $ )' ) TRIM( MESSAGE )
            CALL FLUSH( LOGDEV )
            ANSWER = ' '
            READ  ( *, '( A )', IOSTAT=ISTAT )  ANSWER

!$OMP   END CRITICAL( S_LOGDEV )

        IF ( ISTAT .LT. 0 ) THEN
            CALL M3EXIT( 'M3PROMPT',0,0,'END-OF-FILE encountered', 1 )
        ELSE IF ( ISTAT .EQ. 0 ) THEN
            L = INDEX( ANSWER, '!' )
            IF ( L .GT. 0 ) ANSWER( L: ) = ' '
        END IF

        RETURN
        END  SUBROUTINE M3PROMPT


C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C   begin subroutine  M3PARAG

        SUBROUTINE M3PARAG( NMESG, MSGS )

        IMPLICIT NONE

#ifdef   _AIX
#define  FLUSH flush_
#endif

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'IODECL3.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER      , INTENT(IN   ) :: NMESG
        CHARACTER*(*), INTENT(IN   ) :: MSGS( NMESG )
                 
C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INITLOG3
        EXTERNAL          :: INITBLK3        !!  BLOCK DATA to initialize STATE3 commons

C.......   Scratch LOCAL VARIABLES

        INTEGER     L, M

C......................................................................
C   begin body of subroutine  M3PARAG

!$OMP   CRITICAL( S_LOGDEV )

            IF ( LOGDEV .LT. 0 ) THEN
                LOGDEV = INITLOG3( 'M3MSG2/M3PARAG')
            END IF          !!  if logdev < 0 (initialization required)

            DO L = 1, NMESG
                WRITE( LOGDEV,'( 5X, A )' ) TRIM( MSGS( L ) )
            END DO

#ifdef IOAPI_STDOUT

            IF ( LOGDEV .NE. 6 ) THEN
                DO L = 1, NMESG
                    WRITE( 6,'( 5X, A )' ) TRIM( MSGS( L ) )
                END DO
            END IF          !  if logdev is not pointing at standard output
#endif

        CALL FLUSH( LOGDEV )

!$OMP   END CRITICAL( S_LOGDEV )

        RETURN
        END  SUBROUTINE M3PARAG

C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C   begin subroutine  M3FLUSH

        
        SUBROUTINE M3FLUSH( IDEV )

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: IDEV    !  fortran unit to be flushed

C......................................................................
C   begin body of subroutine  M3flush

!$OMP CRITICAL( S_LOGDEV )

        CALL FLUSH( IDEV )

!$OMP END CRITICAL( S_LOGDEV )

        RETURN
        END SUBROUTINE M3FLUSH


C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
C   begin conditional subroutine  FLUSH

#ifdef IOAPI_NOFLUSH

        SUBROUTINE FLUSH( IDEV )
            IMPLICIT NONE
            INTEGER, INTENT(IN   ) :: IDEV
            !!  present version:  do nothing
            RETURN
        END SUBROUTINE FLUSH

#endif


!!C-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
!!C   begin conditional subroutine  IARGC
!!
!!#ifdef GFORTRAN
!!
!!        INTEGER FUNCTION IARGC(  )
!!            IMPLICIT NONE
!!            INTEGER, EXTERNAL :: GFORTRAN_IARGC
!!            IARGC = GFORTRAN_IARGC()
!!            !!  present version:  do nothing
!!            RETURN
!!        END
!!
!!#endif
