
        SUBROUTINE  PMATVEC11( M, N, P, NX, IX, U, V )

        !!***********************************************************************
        !! Version "$Id: pmatvec.f 1 2017-06-10 18:05:20Z coats $"
        !! EDSS/Models-3 I/O API.
        !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
        !! (C) 2003-2010 by Baron Advanced Meteorological Systems.
        !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
        !! See file "LGPL.txt" for conditions of use.
        !!.........................................................................
        !!  subroutine  PMATVEC11  body starts at line  52:  1-D to 1-D
        !!  subroutine  PMATVEC12  body starts at line  87:  1-D to 2-D
        !!  subroutine  PMATVEC21  body starts at line 147:  2-D to 1-D
        !!  subroutine  PMATVEC22  body starts at line 207:  2-D to 2-D
        !!
        !!  FUNCTION:  multiply a sparse incidence matrix <N,I> by a vector U and
        !!             return the result V
        !!
        !!  PRECONDITIONS REQUIRED:  none
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:  none
        !!
        !!  REVISION  HISTORY:
        !!       prototype 2/95 by CJC
        !!       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
        !!       Version   9/2014 by CJC:  modifications for OpenMP parallel
        !!       Version  12/2014 by CJC for I/O API v3.2:  multiple versions
        !!        with M3UTILIO generic interface PMATVEC()
        !!***********************************************************************

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M           ! length of input vector
        INTEGER, INTENT(IN   ) :: N           ! length of output vector
        INTEGER, INTENT(IN   ) :: P           ! max number of coefficients

        INTEGER, INTENT(IN   ) :: NX( N )      ! # of entries per row
        INTEGER, INTENT(IN   ) :: IX( P )      ! columns list

        REAL, INTENT(IN   ) :: U( M )      !  input vector
        REAL, INTENT(  OUT) :: V( N )      ! output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         R, K
        REAL*8          SUM
        INTEGER         CNT( 0:N )


        !!***********************************************************************
        !!   begin body of subroutine  PMATVEC11

        CNT( 0 ) = 0
        DO  K = 1, N
            CNT( K ) = CNT( K-1 ) + NX( K )
        END DO

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( N, CNT, U, V, IX ),
!$OMP&    PRIVATE( R, SUM, K )

        DO  R = 1, N

            SUM = 0.0d0

            DO  K = CNT( R-1 )+1, CNT( R )
                SUM = SUM  +  U( IX( K ) )
            END DO

            V( R ) = SUM

        END DO

        RETURN

        END  SUBROUTINE  PMATVEC11


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        SUBROUTINE  PMATVEC12( M, NC, NR, P, NX, IX, U, V )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: M           ! length of input vector
        INTEGER, INTENT(IN   ) :: NC, NR      ! length of output vector
        INTEGER, INTENT(IN   ) :: P           ! max number of coefficients

        INTEGER, INTENT(IN   ) :: NX( NC*NR )  ! # of entries per row
        INTEGER, INTENT(IN   ) :: IX( P )      ! columns list

        REAL, INTENT(IN   ) :: U( M )      !  input vector
        REAL, INTENT(  OUT) :: V( NC, NR )      ! output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, J, K
        REAL*8          SUM
        INTEGER         CNT( 0:NC*NR )


        !!***********************************************************************
        !!   begin body of subroutine  PMATVEC11

        CNT( 0 ) = 0
        DO  R = 1, NC*NR
            CNT( R ) = CNT( R-1 ) + NX( R )
        END DO

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( NC, NR, CNT, U, V, IX ),
!$OMP&    PRIVATE( R, SUM, J, K )

        DO  R = 1, NR
        DO  C = 1, NC

            J   = C + (R-1) * NC
            SUM = 0.0d0

            DO  K = CNT( J-1 )+1, CNT( J )
                SUM = SUM  +  U( IX( K ) )
            END DO

            V( C,R ) = SUM

        END DO
        END DO

        RETURN

        END  SUBROUTINE  PMATVEC12


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        SUBROUTINE  PMATVEC21( MC, MR, N, P, NX, IX, U, V )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: MC, MR      ! length of input vector
        INTEGER, INTENT(IN   ) :: N           ! length of output vector
        INTEGER, INTENT(IN   ) :: P           ! max number of coefficients

        INTEGER, INTENT(IN   ) :: NX( N )      ! # of entries per row
        INTEGER, INTENT(IN   ) :: IX( P )      ! columns list

        REAL, INTENT(IN   ) :: U( MC,MR )  !  input vector
        REAL, INTENT(  OUT) :: V( N )      ! output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         R, K, L, CC, RR
        REAL*8          SUM
        INTEGER         CNT( 0:N )


        !!***********************************************************************
        !!   begin body of subroutine  PMATVEC11

        CNT( 0 ) = 0
        DO  R = 1, N
            CNT( R ) = CNT( R-1 ) + NX( R )
        END DO

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( N, CNT, U, V, IX, MC ),
!$OMP&    PRIVATE( R, SUM, L, CC, RR )

        DO  R = 1, N

            SUM = 0.0d0

            DO  K = CNT( R-1 )+1, CNT( R )
                L   = IX( K )
                CC  = MOD( L, MC )
                RR  = 1 + L / MC
                SUM = SUM  +  U( CC,RR )
            END DO

            V( R ) = SUM

        END DO

        RETURN

        END  SUBROUTINE  PMATVEC21


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        SUBROUTINE  PMATVEC22( MC, MR, NC, NR, P, NX, IX, U, V )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: MC, MR      ! length of input vector
        INTEGER, INTENT(IN   ) :: NC, NR      ! length of output vector
        INTEGER, INTENT(IN   ) :: P           ! max number of coefficients

        INTEGER, INTENT(IN   ) :: NX( NC*NR )     ! # of entries per row
        INTEGER, INTENT(IN   ) :: IX( P )        ! columns list

        REAL, INTENT(IN   ) :: U( MC,MR )  !  input vector
        REAL, INTENT(  OUT) :: V( NC,NR )  ! output vector


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, J, K, L, CC, RR
        REAL*8          SUM
        INTEGER         CNT( 0:NC*NR )


        !!***********************************************************************
        !!   begin body of subroutine  PMATVEC11

        CNT( 0 ) = 0
        DO  J = 1, NC*NR
            CNT( J ) = CNT( J-1 ) + NX( J )
        END DO

!$OMP   PARALLEL DO
!$OMP&    DEFAULT( NONE ),
!$OMP&     SHARED( NC, NR, MC, CNT, U, V, IX ),
!$OMP&    PRIVATE( C, R, J, K, L, CC, RR, SUM )

        DO  R = 1, NR
        DO  C = 1, NC

            J   = C + (R-1)*NC
            SUM = 0.0d0

            DO  K = CNT( J-1 )+1, CNT( J )
                L   = IX( K )
                CC  = MOD( L, MC )
                RR  = 1 + L / MC
                SUM = SUM  +  U( CC,RR )
            END DO

            V( C,R ) = SUM

        END DO
        END DO

        RETURN

        END  SUBROUTINE  PMATVEC22



