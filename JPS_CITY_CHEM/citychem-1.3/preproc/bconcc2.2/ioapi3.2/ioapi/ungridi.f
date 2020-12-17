
      SUBROUTINE  UNGRIDIS1( NCOLS1, NROWS1, XORIG, YORIG, XCELL, YCELL,
     &                       NPTS, XLOC, YLOC, NX )

        !!***********************************************************************
        !! Version "$Id: ungridi.f 70 2017-11-30 15:05:43Z coats $"
        !! EDSS/Models-3 I/O API.
        !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
        !! (C) 2003-2010 Baron Advanced Meteorological Systems, and
        !! (C) 2014-2016 UNC Institute for the Environment.
        !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
        !! See file "LGPL.txt" for conditions of use.
        !!.........................................................................
        !!  subroutine body  UNGRIDIS1  starts at line  63
        !!  subroutine body  UNGRIDIS2  starts at line  63
        !!  subroutine body  UNGRIDID1  starts at line  63
        !!  subroutine body  UNGRIDID2  starts at line  63
        !!  subroutine body  UNGRIDI    starts at line  63
        !!
        !!  FUNCTION:
        !! 	computes "ungridding" incidence matrices to be used for program
        !!       METCPLE, etc., to incidence-matrix (conservative precip) re-gridding
        !!	from a grid to a set of locations { <XLOC(S),YLOC(S)>, S=1:NPTS }
        !!
        !!  SEE ALSO:
        !!       UNGRIDB() which computes matrices for bilinear interpolation
        !!       BILIN()   which performs combined interpolate-only,
        !!                 preserving the subscript-order.
        !!       BMATVEC() which performs combined interpolate-and-transpose,
        !!                 e.g., for SMOKE program LAYPOINT, changing LAYER
        !!                 from an outermost subscript to an innermost
        !!
        !!    MODULE M3UTILIO contains a generic UNGRIDI interface that selects
        !!    among UNGRIDIS1, UNGRIDIS2,  UNGRIDID1, UNGRIDID2.
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       USE M3UTILIO
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!       none
        !!
        !!  REVISION  HISTORY:
        !!	    prototype 10/2005 by CJC
        !!      Modified  03/2010 by CJC: F9x changes for I/O API v3.1
        !!      Version  12/2014 by CJC for I/O API v3.2:  OpenMP parallel;
        !!      multiple versions with  M3UTILIO generic interface UNGRIDI()
        !!      Version  03/2016 by CJC:  Add UNGRIDI() for backwards compatibility
        !!      Version  11/2017 by CJC:  bug-fix; Add error-checking forms
        !!***********************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1	!  number of grid columns, rows
        REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
        REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
        INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
        REAL   , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
        REAL   , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
        INTEGER, INTENT(  OUT) :: NX( NPTS )    !  single-indexed subscripts into grid

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER		S		!  source counter
        INTEGER		C, R		!  subscripts into doubly-indexed grid
        REAL*8		DDX, DDY	!  inverse cell size
        REAL*8		X, Y		!  grid-normal coords of point
        REAL*8		XN, YN


        !!***********************************************************************
        !!   begin body of subroutine  UNGRIDB

        DDX = 1.0D0 / XCELL
        DDY = 1.0D0 / YCELL

        XN = DBLE( NCOLS1 )
        YN = DBLE( NROWS1 )

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( DDX, DDY, XORIG, YORIG, XN, YN, NPTS, NX,
!$OMP&             XLOC, YLOC, NCOLS1, NROWS1 ),
!$OMP&    PRIVATE( S, X, Y, C, R )

        DO  S = 1, NPTS

            !!  Hacks to fix this up to deal with the fact
            !!  that computer languages do the WRONG THING
            !!  for negative-number integer conversions and remainders:

            X = DDX * ( XLOC( S ) - XORIG )	!  normalized grid coords
            IF ( X .LT. 0.0D0 ) THEN
                C = 1
            ELSE IF ( X .GT. XN ) THEN
                C = NCOLS1
            ELSE
                C = 1 + INT( X )                ! truncated to integer
            END IF

            Y = DDY * ( YLOC( S ) - YORIG )	!  normalized grid coords
            IF ( Y .LT. 0.0 ) THEN
                R = 1
            ELSE IF ( Y .GT. YN ) THEN
                R = NROWS1
            ELSE
                R = 1 + INT( Y )                ! truncated to integer
            END IF

            NX( S ) = ( R - 1 ) * NCOLS1 + 1

        END DO          !  end matrix computation loop on target locations

        RETURN

      END SUBROUTINE  UNGRIDIS1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE  UNGRIDIS2( NCOLS1, NROWS1, XORIG, YORIG, XCELL, YCELL,
     &                       NCOLS2, NROWS2, XLOC, YLOC, NX )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1    !  number of grid columns, rows
        REAL*8 , INTENT(IN   ) :: XORIG, YORIG      !  X,Y coords of LL grid corner
        REAL*8 , INTENT(IN   ) :: XCELL, YCELL      !  X,Y direction cell size
        INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2    !  number of input-grid locations
        REAL   , INTENT(IN   ) :: XLOC( NCOLS2,NROWS2 ) !  X point coordinates
        REAL   , INTENT(IN   ) :: YLOC( NCOLS2,NROWS2 ) !  Y point coordinates
        INTEGER, INTENT(  OUT) ::   NX( NCOLS2*NROWS2 ) !  single-indexed subscripts into grid

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER		S		!  source counter
        INTEGER		C, R, CC, RR!  subscripts into doubly-indexed grid
        REAL*8		DDX, DDY	!  inverse cell size
        REAL*8		X, Y		!  grid-normal coords of point
        REAL*8		XN, YN


        !!***********************************************************************
        !!   begin body of subroutine  UNGRIDB

        DDX = 1.0D0 / XCELL
        DDY = 1.0D0 / YCELL

        XN = DBLE( NCOLS1 )
        YN = DBLE( NROWS1 )

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( DDX, DDY, XORIG, YORIG, XN, YN, NCOLS2, NROWS2, NX,
!$OMP&             XLOC, YLOC, NCOLS1, NROWS1 ),
!$OMP&    PRIVATE( X, Y, C, R, CC, RR, S )

        DO  RR = 1, NROWS2
        DO  CC = 1, NCOLS2

            !!  Hacks to fix this up to deal with the fact
            !!  that computer languages do the WRONG THING
            !!  for negative-number integer conversions and remainders:

            X = DDX * ( XLOC( CC,RR ) - XORIG )	!  normalized grid coords
            IF ( X .LT. 0.0D0 ) THEN
                C = 1
            ELSE IF ( X .GT. XN ) THEN
                C = NCOLS1
            ELSE
                C = 1 + INT( X )                ! truncated to integer
            END IF

            Y = DDY * ( YLOC( CC,RR ) - YORIG )	!  normalized grid coords
            IF ( Y .LT. 0.0 ) THEN
                R = 1
            ELSE IF ( Y .GT. YN ) THEN
                R = NROWS1
            ELSE
                R = 1 + INT( Y )                ! truncated to integer
            END IF

             S = ( RR - 1 ) * NCOLS2  +  CC

            NX( S ) = ( R - 1 ) * NCOLS1 + 1

        END DO          !  end matrix computation loop on target locations
        END DO          !  end matrix computation loop on target locations

        RETURN

      END SUBROUTINE  UNGRIDIS2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE  UNGRIDID1( NCOLS1, NROWS1, XORIG, YORIG, XCELL, YCELL,
     &                       NPTS, XLOC, YLOC, NX )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1	!  number of grid columns, rows
        REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
        REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
        INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
        REAL*8 , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
        REAL*8 , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
        INTEGER, INTENT(  OUT) :: NX( NPTS )    !  single-indexed subscripts into grid

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER		S		    !  source counter
        INTEGER		C, R		!  subscripts into doubly-indexed grid
        REAL*8		DDX, DDY	!  inverse cell size
        REAL*8		X, Y		!  grid-normal coords of point
        REAL*8		XN, YN


        !!***********************************************************************
        !!   begin body of subroutine  UNGRIDB

        DDX = 1.0D0 / XCELL
        DDY = 1.0D0 / YCELL

        XN = DBLE( NCOLS1 )
        YN = DBLE( NROWS1 )

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( DDX, DDY, XORIG, YORIG, XN, YN, NPTS, NX,
!$OMP&             XLOC, YLOC, NCOLS1, NROWS1 ),
!$OMP&    PRIVATE( S, X, Y, C, R )

        DO  S = 1, NPTS

            !!  Hacks to fix this up to deal with the fact
            !!  that computer languages do the WRONG THING
            !!  for negative-number integer conversions and remainders:

            X = DDX * ( XLOC( S ) - XORIG )	!  normalized grid coords
            IF ( X .LT. 0.0D0 ) THEN
                C = 1
            ELSE IF ( X .GT. XN ) THEN
                C = NCOLS1
            ELSE
                C = 1 + INT( X )                ! truncated to integer
            END IF

            Y = DDY * ( YLOC( S ) - YORIG )	!  normalized grid coords
            IF ( Y .LT. 0.0 ) THEN
                R = 1
            ELSE IF ( Y .GT. YN ) THEN
                R = NROWS1
            ELSE
                R = 1 + INT( Y )                ! truncated to integer
            END IF

            NX( S ) = ( R - 1 ) * NCOLS1 + 1

        END DO          !  end matrix computation loop on target locations

        RETURN

      END SUBROUTINE  UNGRIDID1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE  UNGRIDID2( NCOLS1, NROWS1, XORIG, YORIG, XCELL, YCELL,
     &                       NCOLS2, NROWS2, XLOC, YLOC, NX )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1    !  number of grid columns, rows
        REAL*8 , INTENT(IN   ) :: XORIG, YORIG      !  X,Y coords of LL grid corner
        REAL*8 , INTENT(IN   ) :: XCELL, YCELL      !  X,Y direction cell size
        INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2    !  number of input-grid locations
        REAL*8 , INTENT(IN   ) :: XLOC( NCOLS2,NROWS2 ) !  X point coordinates
        REAL*8 , INTENT(IN   ) :: YLOC( NCOLS2,NROWS2 ) !  Y point coordinates
        INTEGER, INTENT(  OUT) ::   NX( NCOLS2*NROWS2 ) !  single-indexed subscripts into grid

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER		S		!  source counter
        INTEGER		C, R, CC, RR!  subscripts into doubly-indexed grid
        REAL*8		DDX, DDY	!  inverse cell size
        REAL*8		X, Y		!  grid-normal coords of point
        REAL*8		XN, YN


        !!***********************************************************************
        !!   begin body of subroutine  UNGRIDB

        DDX = 1.0D0 / XCELL
        DDY = 1.0D0 / YCELL

        XN = DBLE( NCOLS1 )
        YN = DBLE( NROWS1 )

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( DDX, DDY, XORIG, YORIG, XN, YN, NCOLS2, NROWS2, NX,
!$OMP&             XLOC, YLOC, NCOLS1, NROWS1 ),
!$OMP&    PRIVATE( S, X, Y, C, R, CC, RR )

        DO  RR = 1, NROWS2
        DO  CC = 1, NCOLS2

            !!  Hacks to fix this up to deal with the fact
            !!  that computer languages do the WRONG THING
            !!  for negative-number integer conversions and remainders:

            X = DDX * ( XLOC( CC,RR ) - XORIG )	!  normalized grid coords
            IF ( X .LT. 0.0D0 ) THEN
                C = 1
            ELSE IF ( X .GT. XN ) THEN
                C = NCOLS1
            ELSE
                C = 1 + INT( X )                ! truncated to integer
            END IF

            Y = DDY * ( YLOC( CC,RR ) - YORIG )	!  normalized grid coords
            IF ( Y .LT. 0.0 ) THEN
                R = 1
            ELSE IF ( Y .GT. YN ) THEN
                R = NROWS1
            ELSE
                R = 1 + INT( Y )                ! truncated to integer
            END IF

             S = ( RR - 1 ) * NCOLS2  +  CC

            NX( S ) = ( R - 1 ) * NCOLS1 + 1

        END DO          !  end matrix computation loop on target locations
        END DO          !  end matrix computation loop on target locations

        RETURN

      END SUBROUTINE  UNGRIDID2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



      SUBROUTINE UNGRIDIES1( NCOLS1, NROWS1, XORIG, YORIG, XCELL, YCELL,
     &                       NPTS, XLOC, YLOC, NX, IERR )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1	!  number of grid columns, rows
        REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
        REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
        INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
        REAL   , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
        REAL   , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
        INTEGER, INTENT(  OUT) :: NX( NPTS )    !  single-indexed subscripts into grid
        INTEGER, INTENT(  OUT) :: IERR              !  error-count:  0=="no errors"

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER		S		!  source counter
        INTEGER		C, R		!  subscripts into doubly-indexed grid
        REAL*8		DDX, DDY	!  inverse cell size
        REAL*8		X, Y		!  grid-normal coords of point
        REAL*8		XN, YN

        CHARACTER*256 MESG            !  message/warning buffer

        !!***********************************************************************
        !!   begin body of subroutine  UNGRIDB

        DDX = 1.0D0 / XCELL
        DDY = 1.0D0 / YCELL

        XN = DBLE( NCOLS1 )
        YN = DBLE( NROWS1 )
        
        IERR = 0

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( DDX, DDY, XORIG, YORIG, XN, YN, NPTS, NX,
!$OMP&             XLOC, YLOC, NCOLS1, NROWS1 ),
!$OMP&    PRIVATE( S, X, Y, C, R ),
!$OMP&  REDUCTION( +: IERR )

        DO  S = 1, NPTS

            !!  Hacks to fix this up to deal with the fact
            !!  that computer languages do the WRONG THING
            !!  for negative-number integer conversions and remainders:

            X = DDX * ( XLOC( S ) - XORIG )	!  normalized grid coords
            IF ( X .LT. 0.0D0 ) THEN
                C = 1
                IERR = IERR + 1
            ELSE IF ( X .GT. XN ) THEN
                C = NCOLS1
                IERR = IERR + 1
            ELSE
                C = MIN( NCOLS1, 1 + INT( X ) )                ! truncated to integer
            END IF

            Y = DDY * ( YLOC( S ) - YORIG )	!  normalized grid coords
            IF ( Y .LT. 0.0 ) THEN
                R = 1
                IERR = IERR + 1
            ELSE IF ( Y .GT. YN ) THEN
                R = NROWS1
                IERR = IERR + 1
            ELSE
                R = MIN( NROWS1, 1 + INT( Y ) )                ! truncated to integer
            END IF

            NX( S ) = ( R - 1 ) * NCOLS1 + 1

        END DO          !  end matrix computation loop on target locations

        IF ( IERR .GT. 0 ) THEN
            WRITE( MESG, '( I6, 2X, A )' ) 
     &             IERR, 'points are outside the grid'
            CALL M3WARN( 'UNGRIDI', 0, 0, MESG )
        END IF

        RETURN

      END SUBROUTINE UNGRIDIES1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE UNGRIDIES2( NCOLS1, NROWS1, XORIG, YORIG, XCELL, YCELL,
     &                       NCOLS2, NROWS2, XLOC, YLOC, NX, IERR )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1    !  number of grid columns, rows
        REAL*8 , INTENT(IN   ) :: XORIG, YORIG      !  X,Y coords of LL grid corner
        REAL*8 , INTENT(IN   ) :: XCELL, YCELL      !  X,Y direction cell size
        INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2    !  number of input-grid locations
        REAL   , INTENT(IN   ) :: XLOC( NCOLS2,NROWS2 ) !  X point coordinates
        REAL   , INTENT(IN   ) :: YLOC( NCOLS2,NROWS2 ) !  Y point coordinates
        INTEGER, INTENT(  OUT) ::   NX( NCOLS2*NROWS2 ) !  single-indexed subscripts into grid
        INTEGER, INTENT(  OUT) :: IERR              !  error-count:  0=="no errors"

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER		S		!  source counter
        INTEGER		C, R, CC, RR!  subscripts into doubly-indexed grid
        REAL*8		DDX, DDY	!  inverse cell size
        REAL*8		X, Y		!  grid-normal coords of point
        REAL*8		XN, YN

        CHARACTER*256 MESG            !  message/warning buffer

        !!***********************************************************************
        !!   begin body of subroutine  UNGRIDB

        DDX = 1.0D0 / XCELL
        DDY = 1.0D0 / YCELL

        XN = DBLE( NCOLS1 )
        YN = DBLE( NROWS1 )

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( DDX, DDY, XORIG, YORIG, XN, YN, NCOLS2, NROWS2, NX,
!$OMP&             XLOC, YLOC, NCOLS1, NROWS1 ),
!$OMP&    PRIVATE( X, Y, C, R, CC, RR, S ),
!$OMP&  REDUCTION( +: IERR )

        DO  RR = 1, NROWS2
        DO  CC = 1, NCOLS2

            !!  Hacks to fix this up to deal with the fact
            !!  that computer languages do the WRONG THING
            !!  for negative-number integer conversions and remainders:

            X = DDX * ( XLOC( CC,RR ) - XORIG )	!  normalized grid coords
            IF ( X .LT. 0.0D0 ) THEN
                C = 1
                IERR = IERR + 1
            ELSE IF ( X .GT. XN ) THEN
                C = NCOLS1
                IERR = IERR + 1
            ELSE
                C = MIN( NCOLS1, 1 + INT( X ) )                ! truncated to integer
            END IF

            Y = DDY * ( YLOC( CC,RR ) - YORIG )	!  normalized grid coords
            IF ( Y .LT. 0.0 ) THEN
                R = 1
                IERR = IERR + 1
            ELSE IF ( Y .GT. YN ) THEN
                R = NROWS1
                IERR = IERR + 1
            ELSE
                R = MIN( NROWS1, 1 + INT( Y ) )                ! truncated to integer
            END IF

             S = ( RR - 1 ) * NCOLS2  +  CC

            NX( S ) = ( R - 1 ) * NCOLS1 + 1

        END DO          !  end matrix computation loop on target locations
        END DO          !  end matrix computation loop on target locations

        IF ( IERR .GT. 0 ) THEN
            WRITE( MESG, '( I6, 2X, A )' ) 
     &             IERR, 'points are outside the grid'
            CALL M3WARN( 'UNGRIDI', 0, 0, MESG )
        END IF

        RETURN

      END SUBROUTINE UNGRIDIES2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE UNGRIDIED1( NCOLS1, NROWS1, XORIG, YORIG, XCELL, YCELL,
     &                       NPTS, XLOC, YLOC, NX, IERR )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1	!  number of grid columns, rows
        REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
        REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
        INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
        REAL*8 , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
        REAL*8 , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
        INTEGER, INTENT(  OUT) :: NX( NPTS )    !  single-indexed subscripts into grid
        INTEGER, INTENT(  OUT) :: IERR              !  error-count:  0=="no errors"

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER		S		!  source counter
        INTEGER		C, R		!  subscripts into doubly-indexed grid
        REAL*8		DDX, DDY	!  inverse cell size
        REAL*8		X, Y		!  grid-normal coords of point
        REAL*8		XN, YN

        CHARACTER*256 MESG            !  message/warning buffer

        !!***********************************************************************
        !!   begin body of subroutine  UNGRIDB

        DDX = 1.0D0 / XCELL
        DDY = 1.0D0 / YCELL

        XN = DBLE( NCOLS1 )
        YN = DBLE( NROWS1 )

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( DDX, DDY, XORIG, YORIG, XN, YN, NPTS, NX,
!$OMP&             XLOC, YLOC, NCOLS1, NROWS1 ),
!$OMP&    PRIVATE( S, X, Y, C, R ),
!$OMP&  REDUCTION( +: IERR )

        DO  S = 1, NPTS

            !!  Hacks to fix this up to deal with the fact
            !!  that computer languages do the WRONG THING
            !!  for negative-number integer conversions and remainders:

            X = DDX * ( XLOC( S ) - XORIG )	!  normalized grid coords
            IF ( X .LT. 0.0D0 ) THEN
                C = 1
                IERR = IERR + 1
            ELSE IF ( X .GT. XN ) THEN
                C = NCOLS1
                IERR = IERR + 1
            ELSE
                C = MIN( NCOLS1, 1 + INT( X ) )                ! truncated to integer
            END IF

            Y = DDY * ( YLOC( S ) - YORIG )	!  normalized grid coords
            IF ( Y .LT. 0.0 ) THEN
                R = 1
                IERR = IERR + 1
            ELSE IF ( Y .GT. YN ) THEN
                R = NROWS1
                IERR = IERR + 1
            ELSE
                R = MIN( NROWS1, 1 + INT( Y ) )                ! truncated to integer
            END IF

            NX( S ) = ( R - 1 ) * NCOLS1 + 1

        END DO          !  end matrix computation loop on target locations

        IF ( IERR .GT. 0 ) THEN
            WRITE( MESG, '( I6, 2X, A )' ) 
     &             IERR, 'points are outside the grid'
            CALL M3WARN( 'UNGRIDI', 0, 0, MESG )
        END IF

        RETURN

      END SUBROUTINE UNGRIDIED1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE UNGRIDIED2( NCOLS1, NROWS1, XORIG, YORIG, XCELL, YCELL,
     &                       NCOLS2, NROWS2, XLOC, YLOC, NX, IERR )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1    !  number of grid columns, rows
        REAL*8 , INTENT(IN   ) :: XORIG, YORIG      !  X,Y coords of LL grid corner
        REAL*8 , INTENT(IN   ) :: XCELL, YCELL      !  X,Y direction cell size
        INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2    !  number of input-grid locations
        REAL*8 , INTENT(IN   ) :: XLOC( NCOLS2,NROWS2 ) !  X point coordinates
        REAL*8 , INTENT(IN   ) :: YLOC( NCOLS2,NROWS2 ) !  Y point coordinates
        INTEGER, INTENT(  OUT) ::   NX( NCOLS2*NROWS2 ) !  single-indexed subscripts into grid
        INTEGER, INTENT(  OUT) :: IERR              !  error-count:  0=="no errors"

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER		S		!  source counter
        INTEGER		C, R, CC, RR!  subscripts into doubly-indexed grid
        REAL*8		DDX, DDY	!  inverse cell size
        REAL*8		X, Y		!  grid-normal coords of point
        REAL*8		XN, YN

        CHARACTER*256 MESG            !  message/warning buffer

        !!***********************************************************************
        !!   begin body of subroutine  UNGRIDB

        DDX = 1.0D0 / XCELL
        DDY = 1.0D0 / YCELL

        XN = DBLE( NCOLS1 )
        YN = DBLE( NROWS1 )

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( DDX, DDY, XORIG, YORIG, XN, YN, NCOLS2, NROWS2, NX,
!$OMP&             XLOC, YLOC, NCOLS1, NROWS1 ),
!$OMP&    PRIVATE( S, X, Y, C, R, CC, RR ),
!$OMP&  REDUCTION( +: IERR )

        DO  RR = 1, NROWS2
        DO  CC = 1, NCOLS2

            !!  Hacks to fix this up to deal with the fact
            !!  that computer languages do the WRONG THING
            !!  for negative-number integer conversions and remainders:

            X = DDX * ( XLOC( CC,RR ) - XORIG )	!  normalized grid coords
            IF ( X .LT. 0.0D0 ) THEN
                C = 1
                IERR = IERR + 1
            ELSE IF ( X .GT. XN ) THEN
                C = NCOLS1
                IERR = IERR + 1
            ELSE
                C = MIN( NCOLS1, 1 + INT( X ) )                ! truncated to integer
            END IF

            Y = DDY * ( YLOC( CC,RR ) - YORIG )	!  normalized grid coords
            IF ( Y .LT. 0.0 ) THEN
                R = 1
                IERR = IERR + 1
            ELSE IF ( Y .GT. YN ) THEN
                R = NROWS1
                IERR = IERR + 1
            ELSE
                R = MIN( NROWS1, 1 + INT( Y ) )                ! truncated to integer
            END IF

             S = ( RR - 1 ) * NCOLS2  +  CC

            NX( S ) = ( R - 1 ) * NCOLS1 + 1

        END DO          !  end matrix computation loop on target locations
        END DO          !  end matrix computation loop on target locations

        IF ( IERR .GT. 0 ) THEN
            WRITE( MESG, '( I6, 2X, A )' ) 
     &             IERR, 'points are outside the grid'
            CALL M3WARN( 'UNGRIDI', 0, 0, MESG )
        END IF

        RETURN

      END SUBROUTINE UNGRIDIED2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE  UNGRIDI( NCOLS, NROWS, XORIG, YORIG, XCELL, YCELL,
     &                       NPTS, XLOC, YLOC, NX )

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: NCOLS, NROWS	!  number of grid columns, rows
        REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
        REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
        INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
        REAL   , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
        REAL   , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
        INTEGER, INTENT(  OUT) :: NX( NPTS )    !  single-indexed subscripts into grid
C***********************************************************************

        CALL UNGRIDIS1( NCOLS, NROWS, XORIG, YORIG, XCELL, YCELL,
     &                  NPTS, XLOC, YLOC, NX )

        RETURN
      END SUBROUTINE  UNGRIDI

