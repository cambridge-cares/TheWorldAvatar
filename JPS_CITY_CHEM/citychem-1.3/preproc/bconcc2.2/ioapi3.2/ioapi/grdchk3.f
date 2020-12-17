

        LOGICAL FUNCTION GRDCHK3( FNAME, 
     &                            P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &                            XORIG, YORIG, XCELL, YCELL,
     &                            NLAYS, VGTYP, VGTOP, VGLEV )

C***********************************************************************
C Version "$Id: grdchk3.f 75 2018-01-12 15:48:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 2005-2013 Baron Advanced Meteorological Systems and
C (C) 2007-2018 Carlie J. Coats, Jr.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line 96
C
C  FUNCTION:
C       Checks FDESC3 coordinate and grid description variables
C       against description provided as arguments.
C
C  RETURN VALUE:
C       TRUE iff test is successful
C
C  PRECONDITIONS REQUIRED:
C       Call DESC3( FNAME ) before calling GRDCHK3(...)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:
C       Prototype  9/2004 by CJC
C
C       Version  8/2005:  Bug-fix suggested by Alex Zubrow, UChicago,
C       in WRITE statement at line 164:  strict F77 does not permit
C       concatenation in this context.
C
C       Version  5/2006:  corrected comment after declaration of VGTYP
C       at line 61.  Reported by Jerry Condrey.
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

        IMPLICIT NONE

        !!........  INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'FDESC3.EXT'

        !!........  Arguments:

        CHARACTER*(*), INTENT(IN   ) :: FNAME
        REAL*8       , INTENT(IN   ) :: P_ALP      ! first, second, third map
        REAL*8       , INTENT(IN   ) :: P_BET      ! projection descriptive
        REAL*8       , INTENT(IN   ) :: P_GAM      ! parameters.
        REAL*8       , INTENT(IN   ) :: XCENT      ! lon for coord-system X=0
        REAL*8       , INTENT(IN   ) :: YCENT      ! lat for coord-system Y=0
        REAL*8       , INTENT(IN   ) :: XORIG      ! X-coordinate origin of grid (map units)
        REAL*8       , INTENT(IN   ) :: YORIG      ! Y-coordinate origin of grid
        REAL*8       , INTENT(IN   ) :: XCELL      ! X-coordinate cell dimension
        REAL*8       , INTENT(IN   ) :: YCELL      ! Y-coordinate cell dimension
        INTEGER      , INTENT(IN   ) :: NLAYS      ! number of layers
        INTEGER      , INTENT(IN   ) :: VGTYP      ! vertical coordinate type
        REAL         , INTENT(IN   ) :: VGTOP
        REAL         , INTENT(IN   ) :: VGLEV( * )


        !!........  Local Variables:

        INTEGER         L
        LOGICAL         EFLAG
        CHARACTER*256   MESG


        !!........  Statement Functions:
        !!  It is not reasonable that the two arguments are intended for
        !!  the same thing, even though they may have been calculated
        !!  on different machines, and using different precisions.

        LOGICAL         FLTERR
        REAL            PP, QQ

        LOGICAL         DBLERR
        REAL*8          P, Q

        FLTERR( PP, QQ ) = 
     &  ( (PP - QQ)**2  .GT.  1.0E-9*( PP*PP + QQ*QQ + 1.0E-5 ) )

        DBLERR( P, Q ) = 
     &  ( (P - Q)**2    .GT.  1.0D-10*( P*P + Q*Q + 1.0D-5 ) )


        !!........  begin body ........................................

        EFLAG = .FALSE.

        IF ( DBLERR( P_ALP, P_ALP3D ) ) THEN
            MESG = 'P_ALP mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( P_BET, P_BET3D ) ) THEN
            MESG = 'P_BET mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( P_GAM, P_GAM3D ) ) THEN
            MESG = 'P_GAM mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( XCENT, XCENT3D ) ) THEN
            MESG = 'XCENT mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( YCENT, YCENT3D ) ) THEN
            MESG = 'YCENT mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( XORIG, XORIG3D ) ) THEN
            MESG = 'XORIG mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( YORIG, YORIG3D ) ) THEN
            MESG = 'YORIG mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( XCELL, XCELL3D ) ) THEN
            MESG = 'XCELL mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( YCELL, YCELL3D ) ) THEN
            MESG = 'YCELL mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( NLAYS .GT. 1 .AND. VGTYP .NE. IMISS3 ) THEN

            IF ( VGTYP .NE. VGTYP3D ) THEN
                MESG = 'VGTYP mismatch, file ' // FNAME
                CALL M3MSG2( MESG )
                EFLAG = .TRUE.
            END IF

            IF ( VGTYP .EQ. VGSGPH3 .OR.
     &           VGTYP .EQ. VGSGPN3 .OR.
     &           VGTYP .EQ. VGSIGZ3 .OR.
     &           VGTYP .EQ. VGWRFEM ) THEN

                IF ( FLTERR( VGTOP, VGTOP3D ) ) THEN
                    MESG = 'VGTOP mismatch, file ' // FNAME
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                END IF

            END IF      !  if "sigma" vertical coordinate type

            DO  L = 1, NLAYS+1
                IF ( FLTERR( VGLEV(L), VGLVS3D(L) ) ) THEN
                    WRITE( MESG, '( A, I4, 1X, 2 A )' )  
     &              'Layer', L, 'mismatch, file ', FNAME
                    CALL M3MSG2( MESG )
                    EFLAG = .TRUE.
                END IF
            END DO

        END IF          ! if nlays > 1 and vgtyp not "missing"

        GRDCHK3 = ( .NOT.EFLAG )

        RETURN
        END FUNCTION GRDCHK3
