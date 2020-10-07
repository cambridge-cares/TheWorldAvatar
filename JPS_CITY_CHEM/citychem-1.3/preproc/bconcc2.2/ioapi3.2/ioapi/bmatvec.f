
      SUBROUTINE  BMATVEC01( M, N, P, IX, AX, V, Y )

      !!***********************************************************************
      !! Version "$Id: bmatvec.f 1 2017-06-10 18:05:20Z coats $"
      !! EDSS/Models-3 I/O API.
      !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
      !! (C) 2003-2010 Baron Advanced Meteorological Systems, and
      !! (C) 2014 UNC Institute for the Environment.
      !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
      !! See file "LGPL.txt" for conditions of use.
      !!.........................................................................
      !!  subroutine BMATVEC01 body starts at line  92:  non-layered single-indexed inputs and outputs
      !!  subroutine BMATVEC02      starts at line 113:  non-layered single-indexed inputs, double-indexed outputs
      !!  subroutine BMATVEC11      starts at line 163:  single-indexed inputs and outputs
      !!  subroutine BMATVEC12      starts at line 275:  single-indexed inputs, double-indexed outputs
      !!  subroutine BMATVEC21      starts at line 330:  double-indexed inputs, single-indexed output
      !!  subroutine BMATVEC22      starts at line 382:  double-indexed inputs and output
      !!  subroutine BMATVEC        starts at line 437:  fall-back for non-"USE M3UTILIO"
      !!
      !!  FUNCTION:  apply a 4-band sparse matrix to an array ("layered vector")
      !!             as used for bilinear interpolation of meteorology in LAYPOINT
      !!
      !!  NOTE:  Transposes from I/O subscript order V( M,P )
      !!         to computational subscript order    C( P,N )
      !!
      !!       For bilinear interpolation of gridded data having dimension NC,NR
      !!       to a list of locations having grid-normal coordinates <X(S),Y(S)>:
      !!       let C(S) and R(S) be INT( X(S) ) and INT( Y(S) ), P(S) and Q(S)
      !!       be AMOD( X(S), 1.0 ) and AMOD( Y(S), 1.0 ).  Then IX has the
      !!       single-indexing subscripts into the grid for the cell=corners
      !!       surrounding the <X(S),Y(S)>
      !!           IX(1,S) = C + NC * ( R - 1 ) + 1
      !!           IX(2,S) = C + NC * ( R - 1 )
      !!           IX(3,S) = C + NC *   R
      !!           IX(4,S) = C + NC *   R       + 1
      !!       and AX has the bilinear-interpolation coefficients:
      !!           AX(1,S) = ( 1.0 - P( S ) )*( 1.0 - Q( S ) )
      !!           AX(2,S) =         P( S )  *( 1.0 - Q( S ) )
      !!           AX(3,S) = ( 1.0 - P( S ) )*        Q( S )
      !!           AX(4,S) =         P( S )  *        Q( S )
      !!
      !!  SEE ALSO:
      !!       UNGRIDB() which produces such matrices
      !!       BILIN()   which performs combined interpolate-only,
      !!                 preserving the subscript-order.
      !!
      !!  PRECONDITIONS REQUIRED:
      !!        USE M3UTILIO for generic INTERFACE
      !!        Number of layers same for input and output.
      !!        Index and coefficients set up so that the equation for
      !!        the multiplication (at each row R and layer L) is
      !!
      !!       Y(L,R) = SUM_{ J=1...4 }  A( J,R ) * V( I( J,R ),L )
      !!
      !!  SUBROUTINES AND FUNCTIONS CALLED:  none
      !!
      !!  REVISION  HISTORY:
      !!       prototype 12/1995 by CJC
      !!       Version    9/2014 by CJC:  modifications for OpenMP parallel
      !!        Version  12/2014 by CJC for I/O API v3.2:  multiple versions
      !!        with  M3UTILIO generic interface BMATVEC()
      !!    Version  03/2016 by CJC:  Add BMATVEC() for backwards compatibility
      !!***********************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M               ! length of input  vector
        INTEGER, INTENT(IN   ) :: N               ! length of output vector
        INTEGER, INTENT(IN   ) :: P               ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
        REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( M )          ! non-layered input  vector
        REAL   , INTENT(  OUT) :: Y( N )          ! non-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         S, J1, J2, J3, J4


        !!***********************************************************************
        !!   begin body of subroutine  BMATVEC01

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( N, P, IX, AX, V, Y ),
!$OMP&    PRIVATE( S, J1, J2, J3, J4 )

        DO  S = 1, N

            J1 = IX( 1,S )
            J2 = IX( 2,S )
            J3 = IX( 3,S )
            J4 = IX( 4,S )
            Y( S ) = AX( 1,S ) * V( J1 )  +
     &               AX( 2,S ) * V( J2 )  +
     &               AX( 3,S ) * V( J3 )  +
     &               AX( 4,S ) * V( J4 )

        END DO

        RETURN

      END SUBROUTINE BMATVEC01


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE  BMATVEC02( M, NC, NR, P, IX, AX, V, Y )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M                 ! length of input  vector
        INTEGER, INTENT(IN   ) :: NC, NR            ! dims of output grid
        INTEGER, INTENT(IN   ) :: P                 ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )     ! index array
        REAL   , INTENT(IN   ) :: AX( 4,NC*NR )     ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( M  )           ! non-layered input  vector
        REAL   , INTENT(  OUT) :: Y( NC,NR )        ! non-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, S, J1, J2, J3, J4


        !!***********************************************************************
        !!   begin body of subroutine  BMATVEC01

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( NC, NR, P, IX, AX, V, Y ),
!$OMP&    PRIVATE( C, R, S, J1, J2, J3, J4 )

        DO  R = 1, NR
        DO  C = 1, NC

            S = ( R  - 1 ) * NC  +  C
            J1 = IX( 1,S )
            J2 = IX( 2,S )
            J3 = IX( 3,S )
            J4 = IX( 4,S )
            Y( C,R ) = AX( 1,S ) * V( J1 )  +
     &                 AX( 2,S ) * V( J2 )  +
     &                 AX( 3,S ) * V( J3 )  +
     &                 AX( 4,S ) * V( J4 )

        END DO
        END DO

        RETURN

      END SUBROUTINE BMATVEC02



      SUBROUTINE  BMATVEC11( M, N, P, IX, AX, V, Y )

      !!***********************************************************************
      !! Version "$Id: bmatvec.f 1 2017-06-10 18:05:20Z coats $"
      !! EDSS/Models-3 I/O API.
      !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
      !! (C) 2003-2010 Baron Advanced Meteorological Systems, and
      !! (C) 2014 UNC Institute for the Environment.
      !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
      !! See file "LGPL.txt" for conditions of use.
      !!.........................................................................
      !!  subroutine BMATVEC11 body starts at line  90:  single-indexed inputs and outputs
      !!  subroutine BMATVEC12 body starts at line 114:  single-indexed inputs, double-indexed outputs
      !!  subroutine BMATVEC21 body starts at line 169:  double-indexed inputs, single-indexed output
      !!  subroutine BMATVEC22 body starts at line 221:  double-indexed inputs and output
      !!  subroutine BMATVEC   body starts at line 276:  fall-back for non-"USE M3UTILIO"
      !!
      !!  FUNCTION:  apply a 4-band sparse matrix to an array ("layered vector")
      !!             as used for bilinear interpolation of meteorology in LAYPOINT
      !!
      !!  NOTE:  Transposes from I/O subscript order V( M,P )
      !!         to computational subscript order    C( P,N )
      !!
      !!       For bilinear interpolation of gridded data having dimension NC,NR
      !!       to a list of locations having grid-normal coordinates <X(S),Y(S)>:
      !!       let C(S) and R(S) be INT( X(S) ) and INT( Y(S) ), P(S) and Q(S)
      !!       be AMOD( X(S), 1.0 ) and AMOD( Y(S), 1.0 ).  Then IX has the
      !!       single-indexing subscripts into the grid for the cell=corners
      !!       surrounding the <X(S),Y(S)>
      !!           IX(1,S) = C + NC * ( R - 1 ) + 1
      !!           IX(2,S) = C + NC * ( R - 1 )
      !!           IX(3,S) = C + NC *   R
      !!           IX(4,S) = C + NC *   R       + 1
      !!       and AX has the bilinear-interpolation coefficients:
      !!           AX(1,S) = ( 1.0 - P( S ) )*( 1.0 - Q( S ) )
      !!           AX(2,S) =         P( S )  *( 1.0 - Q( S ) )
      !!           AX(3,S) = ( 1.0 - P( S ) )*        Q( S )
      !!           AX(4,S) =         P( S )  *        Q( S )
      !!
      !!  SEE ALSO:
      !!       UNGRIDB() which produces such matrices
      !!       BILIN()   which performs combined interpolate-only,
      !!                 preserving the subscript-order.
      !!
      !!  PRECONDITIONS REQUIRED:
      !!        USE M3UTILIO for generic INTERFACE
      !!        Number of layers same for input and output.
      !!        Index and coefficients set up so that the equation for
      !!        the multiplication (at each row R and layer L) is
      !!
      !!       Y(L,R) = SUM_{ J=1...4 }  A( J,R ) * V( I( J,R ),L )
      !!
      !!  SUBROUTINES AND FUNCTIONS CALLED:  none
      !!
      !!  REVISION  HISTORY:
      !!       prototype 12/1995 by CJC
      !!       Version    9/2014 by CJC:  modifications for OpenMP parallel
      !!        Version  12/2014 by CJC for I/O API v3.2:  multiple versions
      !!        with  M3UTILIO generic interface BMATVEC()
      !!    Version  03/2016 by CJC:  Add BMATVEC() for backwards compatibility
      !!***********************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M               ! length of input  vector
        INTEGER, INTENT(IN   ) :: N               ! length of output vector
        INTEGER, INTENT(IN   ) :: P               ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
        REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( M,P )        ! P-layered input  vector
        REAL   , INTENT(  OUT) :: Y( P,N )        ! P-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         S, L, J1, J2, J3, J4


        !!***********************************************************************
        !!   begin body of subroutine  BMATVEC11

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( N, P, IX, AX, V, Y ),
!$OMP&    PRIVATE( S, L, J1, J2, J3, J4 )

        DO  S = 1, N

            J1 = IX( 1,S )
            J2 = IX( 2,S )
            J3 = IX( 3,S )
            J4 = IX( 4,S )

            DO  L = 1, P
                Y( L,S ) = AX( 1,S ) * V( J1,L )  +
     &                     AX( 2,S ) * V( J2,L )  +
     &                     AX( 3,S ) * V( J3,L )  +
     &                     AX( 4,S ) * V( J4,L )
            END DO

        END DO

        RETURN

      END SUBROUTINE BMATVEC11


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE  BMATVEC12( M, NC, NR, P, IX, AX, V, Y )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M                 ! length of input  vector
        INTEGER, INTENT(IN   ) :: NC, NR            ! dims of output grid
        INTEGER, INTENT(IN   ) :: P                 ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )     ! index array
        REAL   , INTENT(IN   ) :: AX( 4,NC*NR )     ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( M,P )          ! P-layered input  vector
        REAL   , INTENT(  OUT) :: Y( P,NC,NR )      ! P-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, S, L, J1, J2, J3, J4


        !!***********************************************************************
        !!   begin body of subroutine  BMATVEC11

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( NC, NR, P, IX, AX, V, Y ),
!$OMP&    PRIVATE( C, R, S, L, J1, J2, J3, J4 )

        DO  R = 1, NR
        DO  C = 1, NC

            S = ( R  - 1 ) * NC  +  C
            J1 = IX( 1,S )
            J2 = IX( 2,S )
            J3 = IX( 3,S )
            J4 = IX( 4,S )

            DO  L = 1, P
                Y( L,C,R ) = AX( 1,S ) * V( J1,L )  +
     &                       AX( 2,S ) * V( J2,L )  +
     &                       AX( 3,S ) * V( J3,L )  +
     &                       AX( 4,S ) * V( J4,L )
            END DO

        END DO
        END DO

        RETURN

      END SUBROUTINE BMATVEC12


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE  BMATVEC21( MC, MR, N, P, IX, AX, V, Y )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: MC, MR            ! length of input  vector
        INTEGER, INTENT(IN   ) :: N                 ! dims of output grid
        INTEGER, INTENT(IN   ) :: P                 ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,N )         ! index array
        REAL   , INTENT(IN   ) :: AX( 4,N )         ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( MC,MR,P )      ! P-layered input  vector
        REAL   , INTENT(  OUT) :: Y( P,N )          ! P-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, S, CC, RR, K, L


        !!***********************************************************************
        !!   begin body of subroutine  BMATVEC11

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( N, MC, MR, P, IX, AX, V, Y ),
!$OMP&    PRIVATE( C, R, S, K, L, CC, RR )

        DO  S = 1, N

            K  = IX( 1,S )
            CC = MOD( K, MC )
            RR = 1 + K / MC

            DO  L = 1, P
                Y( L,S ) = AX( 1,S ) * V( CC  ,RR  ,L )  +
     &                     AX( 2,S ) * V( CC+1,RR  ,L )  +
     &                     AX( 3,S ) * V( CC  ,RR+1,L )  +
     &                     AX( 4,S ) * V( CC+1,RR+1,L )
            END DO

        END DO

        RETURN

      END SUBROUTINE BMATVEC21



!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE  BMATVEC22( MC, MR, NC, NR, P, IX, AX, V, Y )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: MC, MR            ! length of input  vector
        INTEGER, INTENT(IN   ) :: NC, NR            ! dims of output grid
        INTEGER, INTENT(IN   ) :: P                 ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )     ! index array
        REAL   , INTENT(IN   ) :: AX( 4,NC*NR )     ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( MC,MR,P )      ! P-layered input  vector
        REAL   , INTENT(  OUT) :: Y( P,NC,NR )      ! P-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, S, CC, RR, K, L


        !!***********************************************************************
        !!   begin body of subroutine  BMATVEC11

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( NC, NR, MC, MR, P, IX, AX, V, Y ),
!$OMP&    PRIVATE( C, R, S, K, L, CC, RR )

        DO  R = 1, NR
        DO  C = 1, NC

            S  = ( R  - 1 ) * NC  +  C
            K  = IX( 1,S )
            CC = MOD( K, MC )
            RR = 1 + K / MC

            DO  L = 1, P
                Y( L,C,R ) = AX( 1,S ) * V( CC  ,RR  ,L )  +
     &                       AX( 2,S ) * V( CC+1,RR  ,L )  +
     &                       AX( 3,S ) * V( CC  ,RR+1,L )  +
     &                       AX( 4,S ) * V( CC+1,RR+1,L )
            END DO

        END DO
        END DO

        RETURN

      END SUBROUTINE BMATVEC22



!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE  BMATVEC021( MC, MR, N, P, IX, AX, V, Y )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: MC, MR            ! length of input  vector
        INTEGER, INTENT(IN   ) :: N                 ! dims of output grid
        INTEGER, INTENT(IN   ) :: P                 ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,N )         ! index array
        REAL   , INTENT(IN   ) :: AX( 4,N )         ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( MC,MR )        ! P-layered input  vector
        REAL   , INTENT(  OUT) :: Y( N )            ! P-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, S, CC, RR, K


        !!***********************************************************************
        !!   begin body of subroutine  BMATVEC11

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( N, MC, MR, P, IX, AX, V, Y ),
!$OMP&    PRIVATE( C, R, S, K, CC, RR )

        DO  S = 1, N

            K  = IX( 1,S )
            CC = MOD( K, MC )
            RR = 1 + K / MC
            Y( S ) = AX( 1,S ) * V( CC  ,RR   )  +
     &               AX( 2,S ) * V( CC+1,RR   )  +
     &               AX( 3,S ) * V( CC  ,RR+1 )  +
     &               AX( 4,S ) * V( CC+1,RR+1 )

        END DO

        RETURN

      END SUBROUTINE BMATVEC021



!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE  BMATVEC022( MC, MR, NC, NR, P, IX, AX, V, Y )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: MC, MR            ! length of input  vector
        INTEGER, INTENT(IN   ) :: NC, NR            ! dims of output grid
        INTEGER, INTENT(IN   ) :: P                 ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )     ! index array
        REAL   , INTENT(IN   ) :: AX( 4,NC*NR )     ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( MC,MR )        ! P-layered input  vector
        REAL   , INTENT(  OUT) :: Y( NC,NR )        ! P-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, S, CC, RR, K


        !!***********************************************************************
        !!   begin body of subroutine  BMATVEC11

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( NC, NR, MC, MR, P, IX, AX, V, Y ),
!$OMP&    PRIVATE( C, R, S, K, CC, RR )

        DO  R = 1, NR
        DO  C = 1, NC

            S  = ( R  - 1 ) * NC  +  C
            K  = IX( 1,S )
            CC = MOD( K, MC )
            RR = 1 + K / MC
            Y( C,R ) = AX( 1,S ) * V( CC  ,RR   )  +
     &                 AX( 2,S ) * V( CC+1,RR   )  +
     &                 AX( 3,S ) * V( CC  ,RR+1 )  +
     &                 AX( 4,S ) * V( CC+1,RR+1 )

        END DO
        END DO

        RETURN

      END SUBROUTINE BMATVEC022



!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE  BMATVEC( M, N, P, IX, AX, V, Y )

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M               ! length of input  vector
        INTEGER, INTENT(IN   ) :: N               ! length of output vector
        INTEGER, INTENT(IN   ) :: P               ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
        REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( M,P )        ! P-layered input  vector
        REAL   , INTENT(  OUT) :: Y( P,N )        ! P-layered output vector

        !!................................................................

        CALL BMATVEC11( M, N, P, IX, AX, V, Y )

        RETURN
      END SUBROUTINE  BMATVEC

