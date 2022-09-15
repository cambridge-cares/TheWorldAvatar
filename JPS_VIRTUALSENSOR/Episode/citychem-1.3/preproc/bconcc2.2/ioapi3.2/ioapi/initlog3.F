
        INTEGER FUNCTION INITLOG3 ( CALLER )

C***********************************************************************
C Version "$Id: initlog3.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  85
C
C  FUNCTION:
C       I/O API INTERNALS ONLY
C       Initialize state for Models-3 I/O.
C       May be called multiple times whenever the caller wants the
C       unit number for the I/O API system log file.
C
C  RETURN VALUE:
C       unit number for the log file
C
C  PRECONDITIONS REQUIRED:
C       Only call from within "!$OMP CRITICAL( S_LOGDEV )" blocks
C
C  REVISION  HISTORY:
C       Adapted 12/2003 by CJC from section of INIT3().
C       Hack "CALL INITBLK3" to deal with failure of SGI version 7.4
C       compilers to follow industry standards.
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'


C...........   ARGUMENT and its description:

        CHARACTER*(*), INTENT(IN   ) :: CALLER


C...........   EXTERNAL FUNCTIONS and their descriptions:

#ifdef   _AIX
#define  FLUSH flush_
#endif

        INTEGER, EXTERNAL :: JUNIT
        EXTERNAL          :: INITBLK3


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*512   EQNAME
        CHARACTER*256   MESG            !  message/warning buffer

        INTEGER         L, IOST

C...........   SAVED LOCAL VARIABLES and their descriptions:
C...........   NOTE:  the ANSI standard requires the use of SAVE statements
C...........   for variables which must retain their values from call to call.

        CHARACTER*16, PARAMETER :: LOGFILE = 'LOGFILE'


C***********************************************************************
C   begin body of subroutine  INITLOG

#if defined(__sgi)    || defined(__mips__)

        !!==========>  *HACK*  to deal with SGI v7.4 or later compilers,
        !!             that do not follow industry standards with respect
        !!             to how to make sure that BLOCK DATA modules are
        !!             properly linked into an executable:

        CALL INITBLK3

#endif

        IF ( LOGDEV .LT. 0 ) THEN

            CALL NAMEVAL( LOGFILE, EQNAME )

            IF ( LOGFILE .EQ. EQNAME(1:16 ) ) THEN

                STDOUT = .TRUE.
                LOGDEV = 6
                IOST   = 0

            ELSE IF ( EQNAME .EQ. '/dev/null' ) THEN

                STDOUT = .FALSE.
                LOGDEV = JUNIT()
                OPEN ( UNIT    =  LOGDEV,
     &                 IOSTAT  =  IOST,
     &                 FILE    =  EQNAME,
     &                 ACCESS  =  'SEQUENTIAL')

            ELSE

                LOGDEV = JUNIT()
                STDOUT = .FALSE.
                OPEN ( UNIT    =  LOGDEV,
     &                 IOSTAT  =  IOST,
     &                 FILE    =  EQNAME,
     &                 STATUS  =  'NEW',
     &                 ACCESS  =  'SEQUENTIAL')

            END IF

            IF ( IOST .NE. 0 ) THEN
                MESG =TRIM( CALLER ) // '/INITLOG3'
                WRITE( *, '( /, 5X, 3 A )' )
     &                    '***  ERROR in ', TRIM( MESG ), '  ***'
                WRITE( *, '( 5X, A, I10 )' )
     &                    'Error opening log file on unit', LOGDEV,
     &                    'I/O STATUS =', IOST
                WRITE( *, '( 5X, 2 A, / )' )
     &                    'File: ', TRIM( EQNAME )
                CALL EXIT( 2 )
            END IF

        END IF          !  if logdev < 0

        INITLOG3 = LOGDEV

        RETURN

        END FUNCTION INITLOG3
