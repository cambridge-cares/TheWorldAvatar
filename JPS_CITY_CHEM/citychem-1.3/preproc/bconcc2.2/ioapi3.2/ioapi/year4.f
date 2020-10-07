
        INTEGER FUNCTION YEAR4 ( YY )

C********************************************************************
C Version "$Id: year4.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (c) 2004-2007 Baron Advanced Meteorological Systems,
C (c) 2007-2013 Carlie J. Coats, Jr., and (C) 2014 UNC Institute
C for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C       function body starts at line  59
C
C  FUNCTION:
C
C      Returns the 4-digit year from the 2-digit year
C       
C
C  REVISION HISTORY:
C
C       Create by M Houyoux: 5/97
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 02/2014 by CJC: Fix MH violation of coding-standards:
C       check status IOS from  ENVINT()!!
C****************************************************************************

        IMPLICIT NONE

C.......   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'IODECL3.EXT'

C.......   ARGUMENTS:

        INTEGER, INTENT(IN   ) :: YY    ! 2 digit year

C.......   EXTERNAL FUNCTIONS

        INTEGER, EXTERNAL :: ENVINT

C.......   LOCAL VARIABLES:

        CHARACTER*256   MESG
        INTEGER         ISTAT

        INTEGER, SAVE :: BASEYR, PIVOTYR
        LOGICAL, SAVE :: FIRSTIME = .TRUE.

        CHARACTER*16, PARAMETER :: PNAME = 'YEAR4'

C......................................................................
C       begin YEAR4

        IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            BASEYR = ENVINT( 'YEAR4_BASE',
     &                        'Base century year for YEAR4 algorithm',
     &                        1900, ISTAT )
            IF ( ISTAT .GT. 0 ) THEN
                CALL M3EXIT( PNAME,0,0,'Bad env vble "YEAR4_BASE"', 2)
            END IF
            IF ( BASEYR .GT. 100 ) THEN
                BASEYR = BASEYR / 100
            END IF
            BASEYR  = BASEYR * 100
            PIVOTYR = ENVINT( 'YEAR4_PIVOT',
     &                        'Pivot year for YEAR4 algorithm',
     &                        BASEYR + 70, ISTAT )
            IF ( ISTAT .GT. 0 ) THEN
                CALL M3EXIT( PNAME,0,0,'Bad env vble "YEAR4_PIVOT"', 2)
            END IF
            PIVOTYR = MOD( PIVOTYR , 100 )
        END IF
        
        IF( YY .GT. BASEYR ) THEN
            YEAR4 = YY
        ELSE IF( YY .GT. 99 .OR. YY .LT. 0 ) THEN
            WRITE( MESG,94010 ) 'Year "', YY, 
     &                          '" is not a 2-digit positive number'
            CALL M3EXIT( 'YEAR4', 0, 0, MESG, 2 )

        ELSE IF( YY .GE. PIVOTYR ) THEN
            YEAR4 = BASEYR + YY
        ELSE
            YEAR4 = BASEYR + 100 + YY
        ENDIF

C................   end body of YEAR4 .......................................

C...........   Internal buffering formats............ 94xxx
 
94010   FORMAT( 10( A, :, I7, :, 1X ) )

        END FUNCTION YEAR4

