
      SUBROUTINE BILIN11L( M, N, P, IX, AX, V, Y )

        !!***********************************************************************
        !! Version "$Id: bilin.f 1 2017-06-10 18:05:20Z coats $"
        !! EDSS/Models-3 I/O API.
        !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
        !! (C) 2003-2010 Baron Advanced Meteorological Systems, and
        !! (C) 2014 UNC Institute for the Environment.
        !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
        !! See file "LGPL.txt" for conditions of use.
        !!.........................................................................
        !!  subroutine body BILIN11L starts at line  88:  layered-vector inputs and outputs
        !!  subroutine body BILIN12L starts at line 142:  layered-vector input, 3-D grid output
        !!  subroutine body BILIN21L starts at line 225:  layered-vector input, 3-D grid output
        !!  subroutine body BILIN22L starts at line 300:  layered-vector input, 3-D grid output
        !!  subroutine body BILIN11  starts at line 380:  non-layered vector inputs and outputs
        !!  subroutine body BILIN12  starts at line 430:  non-layered vector input, 2D grid output
        !!  subroutine body BILIN21  starts at line 483:  2-D grid inputs and output
        !!  subroutine body BILIN22  starts at line 531:  2-D grid inputs and output
        !!  subroutine body BILIN    starts at line 582:  fall-back for non-"USE M3UTILIO"
        !!
        !!  FUNCTION:  apply a 4-band sparse matrix to an array ("layered vector")
        !!
        !!  NOTE:  Maintains I/O subscript order V( M,P )
        !!
        !!       For Bilinear interpolation of gridded data having dimension NC,NR
        !!       to a list of locations having grid-normal coordinates <X(S),Y(S)>:
        !!       let Y(S) and R(S) be INT( X(S) ) and INT( Y(S) ), P(S) and Q(S)
        !!       be AMOD( X(S), 1.0 ) and AMOD( Y(S), 1.0 ).  Then IX has the
        !!	single-indexing subscripts into the grid for the cell=corners
        !!       surrounding the <X(S),Y(S)>
        !!           IX(1,S) = C + NC * ( R - 1 ) + 1
        !!           IX(2,S) = C + NC * ( R - 1 )
        !!           IX(3,S) = C + NC *   R
        !!           IX(4,S) = C + NC *   R       + 1
        !!       and AX has the BILIN1ear-interpolation coefficients:
        !!           AX(1,S) = ( 1.0 - P( S ) )*( 1.0 - Q( S ) )
        !!           AX(2,S) =         P( S )  *( 1.0 - Q( S ) )
        !!           AX(3,S) = ( 1.0 - P( S ) )*        Q( S )
        !!           AX(4,S) =         P( S )  *        Q( S )
        !!
        !!    MODULE M3UTILIO contains a generic BILIN interface that selects
        !!    among the BILIN*
        !!
        !!  SEE ALSO:
        !!       UNGRIDB() which produces such matrices
        !!       BMATVEY() which performs combined interpolate-and-transpose,
        !!                 e.g., for SMOKE program LAYPOINT
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       Number of layers same for input and output.
        !!       Index and coefficients set up so that the equation for
        !!       the multiplication (at each row R and layer L) is
        !!
        !!       Y(L,R) = SUM_{ J=1...4 }  A( J,R ) * V( I( J,R ),L )
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:  none
        !!
        !!  REVISION  HISTORY:
        !!       prototype 8/1999 by CJC
        !!       Version   9/2014 by CJC:  modifications for OpenMP parallel
        !!    Version  12/2014 by CJC for I/O API v3.2:  multiple versions
        !!        with M3UTILIO generic interface BILIN()
        !!***********************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M               ! length of input  vector
        INTEGER, INTENT(IN   ) :: N               ! length of output vector
        INTEGER, INTENT(IN   ) :: P               ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
        REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( M,P )        ! P-layered input  vector
        REAL   , INTENT(  OUT) :: Y( N,P )        ! P-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         R, L, J1, J2, J3, J4


        !!***********************************************************************
        !!   begin body of subroutine  BILIN1

        IF ( L .EQ. 1 ) THEN        !! parallelize on R

!$OMP       PARALLEL DO
!$OMP&        DEFAULT( NONE ),
!$OMP&         SHARED( P, N, IX, AX, V, Y ),
!$OMP&        PRIVATE( R, J1, J2, J3, J4 )

            DO  R = 1, N

                J1 = IX( 1,R )
                J2 = IX( 2,R )
                J3 = IX( 3,R )
                J4 = IX( 4,R )

                Y( R,1 ) = AX( 1,R ) * V( J1,1 )  +
     &                     AX( 2,R ) * V( J2,1 )  +
     &                     AX( 3,R ) * V( J3,1 )  +
     &                     AX( 4,R ) * V( J4,1 )

            END DO

        ELSE        !!  L > 1:  parallelize on L

!$OMP       PARALLEL DO
!$OMP&        DEFAULT( NONE ),
!$OMP&         SHARED( P, N, IX, AX, V, Y ),
!$OMP&        PRIVATE( L, R, J1, J2, J3, J4 )

            DO  L = 1, P
            DO  R = 1, N

                J1 = IX( 1,R )
                J2 = IX( 2,R )
                J3 = IX( 3,R )
                J4 = IX( 4,R )

                Y( R,L ) = AX( 1,R ) * V( J1,L )  +
     &                     AX( 2,R ) * V( J2,L )  +
     &                     AX( 3,R ) * V( J3,L )  +
     &                     AX( 4,R ) * V( J4,L )

            END DO
            END DO

        END IF

        RETURN

      END SUBROUTINE BILIN11L


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE BILIN12L( M, NC, NR, P, IX, AX, V, Y )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M                 ! length of input  vector
        INTEGER, INTENT(IN   ) :: NC, NR            ! dims of output grid
        INTEGER, INTENT(IN   ) :: P                 ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )     ! index array
        REAL   , INTENT(IN   ) :: AX( 4,NC*NR )     ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( M,P )          ! P-layered input  vector
        REAL   , INTENT(  OUT) :: Y( NC,NR,P )      ! P-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, CC, RR, S, K, L, J1, J2, J3, J4


        !!***********************************************************************
        !!   begin body of subroutine  BILIN1

        IF ( L .EQ. 1 ) THEN        !! parallelize on R

!$OMP       PARALLEL DO
!$OMP&        DEFAULT( NONE ),
!$OMP&         SHARED( P, NC, NR, IX, AX, V, Y ),
!$OMP&        PRIVATE( C, R, S, J1, J2, J3, J4 )

            DO  R = 1, NR
            DO  C = 1, NC

                S = ( R  - 1 ) * NC  +  C
                J1 = IX( 1,S )
                J2 = IX( 2,S )
                J3 = IX( 3,S )
                J4 = IX( 4,S )

                Y( C,R,1 ) = AX( 1,S ) * V( J1,1 )  +
     &                       AX( 2,S ) * V( J2,1 )  +
     &                       AX( 3,S ) * V( J3,1 )  +
     &                       AX( 4,S ) * V( J4,1 )

            END DO
            END DO

        ELSE        !!  L > 1:  parallelize on L

!$OMP       PARALLEL DO
!$OMP&        DEFAULT( NONE ),
!$OMP&         SHARED( P, NC, NR, IX, AX, V, Y ),
!$OMP&        PRIVATE( L, C, R, S, J1, J2, J3, J4 )

            DO  L = 1, P
            DO  R = 1, NR
            DO  C = 1, NC

                S = ( R  - 1 ) * NC  +  C
                J1 = IX( 1,S )
                J2 = IX( 2,S )
                J3 = IX( 3,S )
                J4 = IX( 4,S )

                Y( C,R,L ) = AX( 1,S ) * V( J1,L )  +
     &                       AX( 2,S ) * V( J2,L )  +
     &                       AX( 3,S ) * V( J3,L )  +
     &                       AX( 4,S ) * V( J4,L )

            END DO
            END DO
            END DO

        END IF

        RETURN

      END SUBROUTINE BILIN12L


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE BILIN21L( MC, MR, N, P, IX, AX, V, Y )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: MC, MR          ! dims of input  grid
        INTEGER, INTENT(IN   ) :: N               ! dims of output grid
        INTEGER, INTENT(IN   ) :: P               ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
        REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
        REAL   , INTENT(IN   ) ::  V( MC,MR,P )   ! P-layered input  vector
        REAL   , INTENT(  OUT) ::  Y( N,P )       ! P-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, K, L, S, CC, RR


        !!***********************************************************************
        !!   begin body of subroutine  BILIN1

        IF ( L .EQ. 1 ) THEN        !! parallelize on R

!$OMP       PARALLEL DO
!$OMP&        DEFAULT( NONE ),
!$OMP&         SHARED( P, MC, MR, N, IX, AX, V, Y ),
!$OMP&        PRIVATE( S, K, CC, RR )

            DO  S = 1, N

                K  = IX( 1,S )
                CC = MOD( K, MC )
                RR = 1 + K / MC

                Y( S,1 ) = AX( 1,S ) * V( CC  ,RR  ,1 )  +
     &                     AX( 2,S ) * V( CC+1,RR  ,1 )  +
     &                     AX( 3,S ) * V( CC  ,RR+1,1 )  +
     &                     AX( 4,S ) * V( CC+1,RR+1,1 )

            END DO

        ELSE        !!  L > 1:  parallelize on L

!$OMP       PARALLEL DO
!$OMP&        DEFAULT( NONE ),
!$OMP&         SHARED( P, MC, MR, N, IX, AX, V, Y ),
!$OMP&        PRIVATE( L,S, K, CC, RR )

            DO  L = 1, P
            DO  S = 1, N

                K  = IX( 1,S )
                CC = MOD( K, MC )
                RR = 1 + K / MC

                Y( S,L ) = AX( 1,S ) * V( CC  ,RR  ,L )  +
     &                     AX( 2,S ) * V( CC+1,RR  ,L )  +
     &                     AX( 3,S ) * V( CC  ,RR+1,L )  +
     &                     AX( 4,S ) * V( CC+1,RR+1,L )

            END DO
            END DO

        END IF

        RETURN

      END SUBROUTINE BILIN21L


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE BILIN22L( MC, MR, NC, NR, P, IX, AX, V, Y )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: MC, MR          ! dims of input  grid
        INTEGER, INTENT(IN   ) :: NC, NR          ! dims of output grid
        INTEGER, INTENT(IN   ) :: P               ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )   ! index array
        REAL   , INTENT(IN   ) :: AX( 4,NC*NR )   ! 4-band coeff matrix
        REAL   , INTENT(IN   ) ::  V( MC,MR,P )   ! P-layered input  vector
        REAL   , INTENT(  OUT) ::  Y( NC,NR,P )   ! P-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, K, L, S, CC, RR


        !!***********************************************************************
        !!   begin body of subroutine  BILIN1

        IF ( L .EQ. 1 ) THEN        !! parallelize on R

!$OMP       PARALLEL DO
!$OMP&        DEFAULT( NONE ),
!$OMP&         SHARED( P, MC, MR, NC, NR, IX, AX, V, Y ),
!$OMP&        PRIVATE( C, R, S, K, CC, RR )

            DO  R = 1, NR
            DO  C = 1, NC

                S  = ( R  - 1 ) * NC  +  C
                K  = IX( 1,S )
                CC = MOD( K, MC )
                RR = 1 + K / MC

                Y( C,R,1 ) = AX( 1,S ) * V( CC  ,RR  ,1 )  +
     &                       AX( 2,S ) * V( CC+1,RR  ,1 )  +
     &                       AX( 3,S ) * V( CC  ,RR+1,1 )  +
     &                       AX( 4,S ) * V( CC+1,RR+1,1 )

            END DO
            END DO

        ELSE        !!  L > 1:  parallelize on L

!$OMP       PARALLEL DO
!$OMP&        DEFAULT( NONE ),
!$OMP&         SHARED( P, MC, MR, NC, NR, IX, AX, V, Y ),
!$OMP&        PRIVATE( L, C, R, S, K, CC, RR )

            DO  L = 1, P
            DO  R = 1, NR
            DO  C = 1, NC

                S  = ( R  - 1 ) * NC  +  C
                K  = IX( 1,S )
                CC = MOD( K, MC )
                RR = 1 + K / MC

                Y( C,R,L ) = AX( 1,S ) * V( CC  ,RR  ,L )  +
     &                       AX( 2,S ) * V( CC+1,RR  ,L )  +
     &                       AX( 3,S ) * V( CC  ,RR+1,L )  +
     &                       AX( 4,S ) * V( CC+1,RR+1,L )

            END DO
            END DO
            END DO

        END IF

        RETURN

      END SUBROUTINE BILIN22L


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

      SUBROUTINE BILIN11( M, N, IX, AX, V, Y )


        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M               ! length of input  vector
        INTEGER, INTENT(IN   ) :: N               ! length of output vector
        INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
        REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( M )          ! 1-layered input  vector
        REAL   , INTENT(  OUT) :: Y( N )          ! 1-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         R, L, J1, J2, J3, J4


        !!***********************************************************************
        !!   begin body of subroutine  BILIN11

!$OMP       PARALLEL DO
!$OMP&        DEFAULT( NONE ),
!$OMP&         SHARED( N, IX, AX, V, Y ),
!$OMP&        PRIVATE( R, J1, J2, J3, J4 )

        DO  R = 1, N

            J1 = IX( 1,R )
            J2 = IX( 2,R )
            J3 = IX( 3,R )
            J4 = IX( 4,R )

            Y( R ) = AX( 1,R ) * V( J1 )  +
     &               AX( 2,R ) * V( J2 )  +
     &               AX( 3,R ) * V( J3 )  +
     &               AX( 4,R ) * V( J4 )

        END DO

        RETURN

      END SUBROUTINE BILIN11


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE BILIN12( M, NC, NR, IX, AX, V, Y )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M                 ! length of input  vector
        INTEGER, INTENT(IN   ) :: NC, NR            ! dims of output grid
        INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )     ! index array
        REAL   , INTENT(IN   ) :: AX( 4,NC*NR )     ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( M )            ! 1-layered input  vector
        REAL   , INTENT(  OUT) :: Y( NC,NR )        ! 1-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, CC, RR, S, K, L, J1, J2, J3, J4


        !!***********************************************************************
        !!   begin body of subroutine  BILIN11

!$OMP       PARALLEL DO
!$OMP&        DEFAULT( NONE ),
!$OMP&         SHARED( NC, NR, IX, AX, V, Y ),
!$OMP&        PRIVATE( C, R, S, J1, J2, J3, J4 )

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

      END SUBROUTINE BILIN12


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE BILIN21( MC, MR, N, IX, AX, V, Y )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: MC, MR          ! dims of input  grid
        INTEGER, INTENT(IN   ) :: N               ! dims of output grid
        INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
        REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
        REAL   , INTENT(IN   ) ::  V( MC,MR )     ! nonlayered input  vector
        REAL   , INTENT(  OUT) ::  Y( N )         ! nonlayered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, K, L, S, CC, RR


        !!***********************************************************************
        !!   begin body of subroutine  BILIN11

!$OMP       PARALLEL DO
!$OMP&        DEFAULT( NONE ),
!$OMP&         SHARED( MC, MR, N, IX, AX, V, Y ),
!$OMP&        PRIVATE( C, R, S, K, CC, RR )

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

      END SUBROUTINE BILIN21


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE BILIN22( MC, MR, NC, NR, IX, AX, V, Y )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: MC, MR          ! dims of input  grid
        INTEGER, INTENT(IN   ) :: NC, NR          ! dims of output grid
        INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )   ! index array
        REAL   , INTENT(IN   ) :: AX( 4,NC*NR )   ! 4-band coeff matrix
        REAL   , INTENT(IN   ) ::  V( MC,MR )     ! P-layered input  vector
        REAL   , INTENT(  OUT) ::  Y( NC,NR )     ! P-layered output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, K, L, S, CC, RR


        !!***********************************************************************
        !!   begin body of subroutine  BILIN11

!$OMP       PARALLEL DO
!$OMP&        DEFAULT( NONE ),
!$OMP&         SHARED( MC, MR, NC, NR, IX, AX, V, Y ),
!$OMP&        PRIVATE( C, R, S, K, CC, RR )

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

      END SUBROUTINE BILIN22


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


      SUBROUTINE BILIN( M, N, P, IX, AX, V, Y )

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M               ! length of input  vector
        INTEGER, INTENT(IN   ) :: N               ! length of output vector
        INTEGER, INTENT(IN   ) :: P               ! number of layers
        INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
        REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
        REAL   , INTENT(IN   ) :: V( M,P )        ! P-layered input  vector
        REAL   , INTENT(  OUT) :: Y( N,P )        ! P-layered output vector

        CALL BILIN11L( M, N, P, IX, AX, V, Y )

        RETURN

      END SUBROUTINE BILIN


