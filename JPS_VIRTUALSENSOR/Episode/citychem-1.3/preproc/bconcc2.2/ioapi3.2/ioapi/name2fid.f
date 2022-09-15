
        INTEGER FUNCTION NAME2FID( FNAME )

C***********************************************************************
C Version "$Id: name2fid.f 107 2018-07-26 14:05:39Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function  NAME2FID   starts at line  66
C
C  FUNCTION:
C       Find STATE3 index FID for  I/O API file with logical name FNAME.
C       SIDE EFFECT:  calls INIT3() in a thread-safe manner.
C
C  RETURN VALUE:
C       TSTATE3 index FID if the file with logical name FNAME has been opened.
C        -1 if I/O API not yet initialized, or if FNAME too long;
C         0 if FNAME not yet opened.
C
C  PRECONDITIONS REQUIRED:
C       none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       INDEX1, TRIMLEN
C
C  REVISION  HISTORY:  
C       Prototype 10/2003 by CJC for I/O API version 3
C       Bug-fix   11/2003 by CJC:  don't return from within critical section
C       Modified  3/2005 by CJC:  bad name length returns  (-1)
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

        IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'IODECL3.EXT'
        INCLUDE 'STATE3.EXT'


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME   !  logical name of file to be opened


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL ::INDEX1


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*16    FIL16           !  scratch file-name buffer
        CHARACTER*256   MESG
        INTEGER         L
        LOGICAL         EFLAG


C***********************************************************************
C   begin body of function  NAME2FID

        EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
        IF ( .NOT. FINIT3 ) THEN
            L = INIT3()
            EFLAG    = .TRUE.
        END IF
!$OMP   END CRITICAL( S_INIT )

        IF ( EFLAG ) THEN
            CALL M3MSG2( 'I/O API not yet initialized' )
            NAME2FID = 0
            EFLAG    = .TRUE.
            RETURN
        END IF

        L = LEN_TRIM( FNAME )
        IF ( L .GT. NAMLEN3 ) THEN
            WRITE( MESG, '( 3 A, I9, A, I9 )' )
     &          'Bad file name length for "', FNAME( 1:L ),
     &          '":  max length ', NAMLEN3, ' actual ', L
            CALL M3MSG2( MESG )
            NAME2FID = -1
            RETURN
        END IF
        

C.......   Find STATE3 index for the file:

        FIL16    = FNAME   !  fixed-length-16 scratch copy of name
        NAME2FID = INDEX1( FIL16, COUNT3, FLIST3 )

        RETURN

        END FUNCTION NAME2FID


