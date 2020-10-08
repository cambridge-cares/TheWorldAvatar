
        LOGICAL FUNCTION PROMPTGRID()

C***********************************************************************
C Version "$Id: promptgrid.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line 52
C
C  RETURNS:  TRUE iff success
C
C  FUNCTION:
C       Prompts user repeatedly for grid/coordinate system name, then
C       uses DSCGRID or DSCOORD to put grid/coordinate system description
C       into FDESC3.EXT data structures.
C
C  PRECONDITIONS REQUIRED:
C       Valid GRIDDESC file
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       DSCOORD, DSCGRID, GETSTR, GETYN, M3WARN
C
C  REVISION  HISTORY:
C       prototype 11/95 by CJC
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'PARMS3.EXT'      ! I/O API constants
      INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure

C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: DSCOORD, DSCGRID, GETYN

C...........   Local Variables:

        CHARACTER*16  	ANAME, CNAME

C***********************************************************************
C   begin body of function  PROMPTGRID

11      CONTINUE
            
            CALL GETSTR( 'Enter name for output coordinate system',
     &                   'UTM_17', GDNAM3D )

            IF ( DSCOORD( GDNAM3D, GDTYP3D, 
     &                          P_ALP3D, P_BET3D, P_GAM3D, 
     &                          XCENT3D, YCENT3D ) ) THEN

                XORIG3D = DBLE( BADVAL3 )
                YORIG3D = DBLE( BADVAL3 )
                XCELL3D = DBLE( BADVAL3 )
                YCELL3D = DBLE( BADVAL3 )
                NCOLS3D = IMISS3
                NROWS3D = IMISS3
                NTHIK3D = IMISS3

                PROMPTGRID = .TRUE.

            ELSE IF ( DSCGRID( GDNAM3D, ANAME, GDTYP3D,     !  retry with dscgrid()
     &                         P_ALP3D, P_BET3D, P_GAM3D, 
     &                         XCENT3D, YCENT3D, XORIG3D, YORIG3D, 
     &                         XCELL3D, YCELL3D, 
     &                         NCOLS3D, NROWS3D, NTHIK3D ) ) THEN

                PROMPTGRID = .TRUE.

            ELSE

                WRITE( *,'( 5X, A )' ) 
     &              'Could not get description for coordinate system "'
     &              // TRIM( CNAME ) // '"'

                IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                    GO TO  11
                ELSE
                    CALL M3EXIT( 'PROMPTGRID', 0, 0,
     &                           'Bad grid/coordinate system', 2 )
                END IF      !  if retry getstr() or not

            END IF          !  if dscoord() failed; end of loop

        GDNAM3D = CNAME

        RETURN

        END FUNCTION PROMPTGRID

