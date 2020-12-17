
        LOGICAL FUNCTION FLUSH3( FNAME )

C***********************************************************************
C Version "$Id: flush3.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2011 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function  FLUSH3   starts at line   68
C
C  FUNCTION:
C       Flushes I/O API file with logical name FNAME.
C
C  RETURN VALUE:
C       TRUE iff it succeeds.
C
C  PRECONDITIONS REQUIRED:
C       I/O API already initialized.
C       File with logical name FNAME exists and has been opened
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       NAME2FID
C       SYNCFID
C
C  REVISION  HISTORY:  
C       prototype 8/1995 by CJC
C
C       Modified  5/1998 by CJC for OpenMP thread-safety
C
C       Modified  5/1998 by CJC:  removed unused local variable "V"
C
C       Modified 10/2003 by CJC for I/O API version 3:
C       Structure in terms of new LOGICAL SYNCFID, INTEGER NAME2FID;
C       support for native-binary BINFILE3 and LISTFIL3 file types
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 08/2015 by CJC: call SYNCFID()
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME   !  logical name of file to be closed


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: NAME2FID        !  fname~~> fid lookup
        LOGICAL, EXTERNAL :: SYNCFID


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FILE            !  file index
        CHARACTER*256   MESG


C***********************************************************************
C   begin body of function  FLUSH3

C.......   Find STATE3 index for the file; then call SYNCFID:

        FILE = NAME2FID( FNAME )

        IF ( FILE .EQ. 0 ) THEN !  file not open.
            MESG = 'FLUSH3:  invalid file "' // FNAME // '"'
            CALL M3MSG2( MESG )
            FLUSH3 = .FALSE.
            RETURN
        END IF

        FLUSH3 = SYNCFID( FILE )
        RETURN

        END FUNCTION FLUSH3

