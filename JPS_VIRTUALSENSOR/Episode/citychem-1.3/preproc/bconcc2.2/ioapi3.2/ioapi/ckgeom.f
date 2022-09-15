
        LOGICAL FUNCTION CKGEOM( FILE, GRID ) RESULT( CKFLAG )

C***********************************************************************
C Version "$Id: ckgeom.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2011 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and 
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  91
C
C  DESCRIPTION:
C       Check consistency of horizontal grid description of the specified
C       file and the specified GRIDDESC grid.  
C       Log any inconsistencies found.
C       Return .TRUE. iff the two are consistent
C
C  PRECONDITIONS REQUIRED:
C       setenv GRIDDESC <path-name for GRIDDESC file>
C       FILE must be the logical name of a file already opened via OPEN3()
C       GRID must be a valid grid described within the GRIDDESC file
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       DSCGRID
C
C  REVISION  HISTORY:
C       Prototype 8/99 by Carlie J. Coats, Jr., NCSC
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO
C***********************************************************************

        USE M3UTILIO

        IMPLICIT NONE

C...........   INCLUDES:

      INCLUDE 'STATE3.EXT'      ! I/O API state data structure


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FILE    !  logical name of file to check
        CHARACTER*(*), INTENT(IN   ) :: GRID    !  GRIDDESC name of grid to check

C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*256   MESG
        LOGICAL         EFLAG

        CHARACTER*16    CNAME   !  scratch  coordinate-system-name buffer
        INTEGER         FID     !  subscript for STATE3 arrays

        INTEGER         NCOLS
        INTEGER         NROWS
        INTEGER         NTHIK
        INTEGER         GDTYP
        REAL*8          P_ALP      ! first, second, third map
        REAL*8          P_BET      ! projection descriptive
        REAL*8          P_GAM      ! parameters.
        REAL*8          XCENT      ! lon for coord-system X=0
        REAL*8          YCENT      ! lat for coord-system Y=0
        REAL*8          XORIG      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG      ! Y-coordinate origin of grid
        REAL*8          XCELL      ! X-coordinate cell dimension
        REAL*8          YCELL      ! Y-coordinate cell dimension

C...........   STATEMENT FUNCTION:  REAL*8 "definitely unequal"
        
        LOGICAL         DBLERR
        REAL*8          P, Q

        DBLERR( P, Q ) = 
     &      ( (P - Q)**2  .GT.  1.0E-10*( P*P + Q*Q + 1.0E-5 ) )

C***********************************************************************
C   begin body of program CKGEOM

C.......   Check that Models-3 I/O has been initialized:
 
        EFLAG = .FALSE.
!$OMP   CRITICAL( S_INIT )
        IF ( .NOT. FINIT3 ) THEN
            LOGDEV = INIT3()
            EFLAG = .TRUE.
        END IF          !  if not FINIT3
!$OMP   END CRITICAL( S_INIT )
        IF ( EFLAG ) THEN
            CALL M3MSG2(  'CKGEOM:  I/O API not yet initialized.' )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( LEN( FILE ) .GT. 16 ) THEN
            WRITE( MESG, '( 3 A, I4 )' )
     &      'CKGEOM:  For file "', FILE,
     &      '": max file name length 16; actual:', LEN( FILE )
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  if len( FILE ) > 16, or if len( vname ) > 16

        IF ( LEN( GRID ) .GT. 16 ) THEN
            WRITE( MESG, '( 3 A, I4 )' )
     &      'CKGEOM:  For grid "', GRID, 
     &      '": max name length 16; actual:', LEN( GRID )
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          !  if len( grid ) > 16, or if len( vname ) > 16

        FID   = INDEX1( FILE, COUNT3, FLIST3 )

        IF ( FID .LE. 0 ) THEN  !  file not open.
            MESG = 'CKGEOM:  File "' // FILE // '" not yet opened'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF          

        IF ( EFLAG ) THEN
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( .NOT. DSCGRID( GRID, CNAME, GDTYP, 
     &              P_ALP, P_BET,P_GAM, XCENT, YCENT,
     &              XORIG, YORIG, XCELL, YCELL, 
     &              NCOLS, NROWS, NTHIK ) ) THEN
            MESG = 'CKGEOM:  Grid "' // GRID // 
     &             '" not in GRIDDESC'
            CALL M3MSG2( MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF                  ! if dscgrid() failed

        IF ( GDTYP .NE. GDTYP3( FID ) ) THEN
            MESG = 'CKGEOM:  GDTYP mismatch, file "' // FILE // 
     &             '", grid "' // GRID // '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( NCOLS .NE. NCOLS3( FID ) ) THEN
            MESG = 'CKGEOM:  NCOLS mismatch, file "' // FILE // 
     &             '", grid "' // GRID // '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( NROWS .NE. NROWS3( FID ) ) THEN
            MESG = 'CKGEOM:  NROWS mismatch, file "' // FILE // 
     &             '", grid "' // GRID // '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF
             
        IF ( NTHIK .NE. NTHIK3( FID ) ) THEN
            MESG = 'CKGEOM:  NTHIK mismatch, file "' // FILE // 
     &             '", grid "' // GRID // '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF
             
        IF ( DBLERR( P_ALP, P_ALP3( FID ) ) ) THEN
            MESG = 'CKGEOM:  P_ALP mismatch, file "' // FILE // 
     &             '", grid "' // GRID // '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( P_BET, P_BET3( FID ) ) ) THEN
            MESG = 'CKGEOM:  P_BET mismatch, file "' // FILE // 
     &             '", grid "' // GRID // '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( P_GAM, P_GAM3( FID ) ) ) THEN
            MESG = 'CKGEOM:  P_GAM mismatch, file "' // FILE // 
     &             '", grid "' // GRID // '"' 
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF
             
        IF ( DBLERR( XCENT, XCENT3( FID ) ) ) THEN
            MESG = 'CKGEOM:  XCENT mismatch, file "' // FILE // 
     &             '", grid "' // GRID // '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF
             
        IF ( DBLERR( YCENT, XCENT3( FID ) ) ) THEN
            MESG = 'CKGEOM:  YCENT mismatch, file "' // FILE // 
     &             '", grid "' // GRID // '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF
        
        IF ( DBLERR( XORIG, XORIG3( FID ) ) ) THEN
            MESG = 'CKGEOM:  XORIG mismatch, file "' // FILE // 
     &             '", grid "' // GRID // '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF
             
        IF ( DBLERR( YORIG, XORIG3( FID ) ) ) THEN
            MESG = 'CKGEOM:  YORIG mismatch, file "' // FILE // 
     &             '", grid "' // GRID // '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        CKFLAG = ( .NOT. EFLAG )

        END FUNCTION CKGEOM

