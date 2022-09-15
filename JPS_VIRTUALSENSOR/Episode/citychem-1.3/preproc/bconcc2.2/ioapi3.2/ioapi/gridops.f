
        SUBROUTINE GRIDOPS( NCOL, NROW, NSPC, NLEV, A, B, C )

C***********************************************************************
C Version "$Id: gridops.f 27 2017-10-13 17:52:55Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2011 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and 
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine GRIDOPS body starts at line  132
C  entry      PICKOPS body starts at line  347
C  entry      NAMEDOP body starts at line  362
C
C  PRECONDITIONS REQUIRED:  Call entry PICKOPS before calling GRIDOPS
C                           Valid OPNAME for NAMEDOP
C
C  FUNCTION:   Entry PICKOPS is used to set state variable DIFMODE, and
C     returns the name of the corresponding operation.
C
C     entry NAMEDOP takes an operation name (from PICKOPS), and a full set
C     of GRIDOPS arguments, selects the appropriate DIFMODE, and goes to
C     the head of GRIDOPS.
C
C     main routine GRIDOPS generates C = A "op" B, where "op" is an operation
C     defined in terms of state variable DIFMODE as follows:
C
C   1:   (pointwise) difference                             A - B
C   2:   (pointwise) difference                             B - A
C   3:   (pointwise) ratio                                  A / B
C   4:   (pointwise) ratio                                  B / A
C   5:   (pointwise) absolute value of difference          |A - B|
C   6:   difference normalized by first grid               (A - B)/A
C   7:   difference normalized by second grid              (B - A)/B
C   8:   difference normalized by second grid              (A - B)/B
C   9:   absolute value of difference normalized by A      |A - B|/A
C  10:   absolute value of difference normalized by B      |A - B|/B
C  11:   difference normalized by pointwise mean          2(A - B)/(A + B)
C  12:   difference normalized by pointwise mean          2(B - A)/(A + B)
C  13:   difference normalized by joint root mean square   (A - B)/RMS(A&B)
C  14:   difference normalized by joint root mean square   (B - A)/RMS(A&B)
C  15:   (pointwise) sum                                    A + B
C  16:   (pointwise) maximum                                MAX( A,B )
C  17:   (pointwise) minimum                                min( A,B )
C  18:   value from grid A                                  A
C  19:   value from grid B                                  B
C
C  SUBROUTINES AND FUNCTIONS CALLED:  M3EXIT, GETMENU, INDEX1
C
C  REVISION  HISTORY:
C       prototype 09/1992 by CJC
C       Modified  03/2010 by CJC: F9x changes for I/O API v3.1
C       Modified  02/2015 by CJC for I/O API 3.2: USE M3UTILIO
C       Modified  10/2017 by CJC for I/O API 3.2: bugfix for Modes 13,14,17
C***********************************************************************

        USE M3UTILIO

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        INTEGER     , INTENT(IN   ) :: NCOL, NROW, NSPC, NLEV          !  dimensions
        REAL        , INTENT(IN   ) :: A( NCOL*NROW*NSPC*NLEV )        !  first  input grid
        REAL        , INTENT(IN   ) :: B( NCOL*NROW*NSPC*NLEV )        !  second input grid
        REAL        , INTENT(  OUT) :: C( NCOL*NROW*NSPC*NLEV )        !  output grid
        CHARACTER*16, INTENT(INOUT) :: OPNAME                          !  for PICKOPS


C...........   Parameter:

        INTEGER, PARAMETER :: OPCOUNT = 19         !  dimension for OP(*); number of ops
        REAL   , PARAMETER :: MISSING = BADVAL3    !  fill value for zero-divide cells

        CHARACTER*72, PARAMETER :: DIFMNU ( OPCOUNT ) = (/
     &'(pointwise) difference                           A - B        ',        !  1
     &'(pointwise) difference                           B - A        ',        !  2
     &'(pointwise) ratio                                A / B        ',        !  3
     &'(pointwise) ratio                                B / A        ',        !  4
     &'(pointwise) absolute value of difference        |A - B|       ',        !  5
     &'difference normalized by first grid             (A - B)/A     ',        !  6
     &'difference normalized by second grid            (B - A)/B     ',        !  7
     &'difference normalized by second grid            (A - B)/B     ',        !  8
     &'absolute value of difference normalized by A    |A - B|/A     ',        !  9
     &'absolute value of difference normalized by B    |A - B|/B     ',        ! 10
     &'difference normalized by pointwise mean        2(A-B)/(A + B) ',        ! 11
     &'difference normalized by pointwise mean        2(B-A)/(A + B) ',        ! 12
     &'difference normalized by joint root mean square (A-B)/RMS(A&B)',        ! 13
     &'difference normalized by joint root mean square (B-A)/RMS(A&B)',        ! 14
     &'(pointwise) sum                                  A + B        ',        ! 15
     &'(pointwise) maximum                              MAX( A,B )   ',        ! 16
     &'(pointwise) minimum                              min( A,B )   ',        ! 17
     &'value from grid A                                A            ',        ! 18
     &'value from grid B                                B            '         ! 19
     & /)

        CHARACTER*16, PARAMETER :: OP ( OPCOUNT ) = (/
     &          '(A - B)         ' ,         !  1
     &          '(B - A)         ' ,         !  2
     &          'A / B           ' ,         !  3
     &          'B / A           ' ,         !  4
     &          '|(A - B)|       ' ,         !  5
     &          '(A - B)/A       ' ,         !  6
     &          '(B - A)/B       ' ,         !  7
     &          '(A - B)/B       ' ,         !  8
     &          '|(A - B)/A|     ' ,         !  9
     &          '|(B - A)/B|     ' ,         ! 10
     &          '(A-B)/((A+B)/2) ' ,         ! 11
     &          '(B-A)/((A+B)/2) ' ,         ! 12
     &          '(A - B)/RMS     ' ,         ! 13
     &          '(B - A)/RMS     ' ,         ! 14
     &          '(A + B)         ' ,         ! 15
     &          'MAX(A, B)       ' ,         ! 16
     &          'min(a, B)       ' ,         ! 17
     &          'Grid A          ' ,         ! 18
     &          'Grid B          ' /)        ! 19


C...........   LOCAL VARIABLES:  menu choices and descriptions

        INTEGER, SAVE :: DIFMODE = 1       !  operation selected from DIFMNU

        INTEGER       I, J, K         !  loop counters
        REAL          S, T, U, V      !  scratch variables

C***********************************************************************
C   begin body of subroutine  GRIDOPS

1       CONTINUE        !  from entry NAMEDOP

        IF      ( DIFMODE .EQ. 1 )  THEN        !  diff

            DO 11  I = 1, NCOL*NROW*NSPC*NLEV
                 C( I ) = A( I ) - B( I )
11          CONTINUE

        ELSE IF ( DIFMODE .EQ. 2 )  THEN        !  diff B-A

            DO 22  I = 1, NCOL*NROW*NSPC*NLEV
                 C( I ) = B( I ) - A( I )
22          CONTINUE

        ELSE IF ( DIFMODE .EQ. 3 )  THEN        !  ratio A/B

            DO 33  I = 1, NCOL*NROW*NSPC*NLEV
                 T = B( I )
                 IF ( T .NE. 0.0 ) THEN
                     C( I ) = A( I ) / T
                 ELSE
                     C( I ) = MISSING
                 END IF
33          CONTINUE

        ELSE IF ( DIFMODE .EQ. 4 )  THEN        !  ratio B/A

            DO 44  I = 1, NCOL*NROW*NSPC*NLEV
                 T = A( I )
                 IF ( T .NE. 0.0 ) THEN
                     C( I ) = B( I ) / T
                 ELSE
                     C( I ) = MISSING
                 END IF
44          CONTINUE

        ELSE IF ( DIFMODE .EQ. 5 )  THEN        !  |diff A_B |

            DO 55  I = 1, NCOL*NROW*NSPC*NLEV
                 C( I ) = ABS( A( I ) - B( I ) )
55          CONTINUE

        ELSE IF ( DIFMODE .EQ. 6 )  THEN        !  diffn A-B/A

            DO 66  I = 1, NCOL*NROW*NSPC*NLEV
                 T = A( I )
                 IF ( T .NE. 0.0 ) THEN
                     C( I ) = ( T - B( I ) ) / T
                 ELSE
                     C( I ) = MISSING
                 END IF
66          CONTINUE

        ELSE IF ( DIFMODE .EQ. 7 )  THEN        !  diffn B-A/B

            DO 77  I = 1, NCOL*NROW*NSPC*NLEV
                 T = B( I )
                 IF ( T .NE. 0.0 ) THEN
                     C( I ) = ( T - A( I ) ) / T
                 ELSE
                     C( I ) = MISSING
                 END IF
77          CONTINUE

        ELSE IF ( DIFMODE .EQ. 8 )  THEN        !  diffn A-B/B

            DO 88  I = 1, NCOL*NROW*NSPC*NLEV
                 T = B( I )
                 IF ( T .NE. 0.0 ) THEN
                     C( I ) = ( A( I ) - T ) / T
                 ELSE
                     C( I ) = MISSING
                 END IF
88          CONTINUE

        ELSE IF ( DIFMODE .EQ. 9 )  THEN        !  diffna |A - B|/A

            DO 99  I = 1, NCOL*NROW*NSPC*NLEV
                 T = A( I )
                 IF ( T .NE. 0.0 ) THEN
                     C( I ) = ABS( ( T - B( I ) ) / T )
                 ELSE
                     C( I ) = MISSING
                 END IF
99          CONTINUE

        ELSE IF ( DIFMODE .EQ. 10 )  THEN        !  diffna |A - B|/B

            DO 101  I = 1, NCOL*NROW*NSPC*NLEV
                 T = B( I )
                 IF ( T .NE. 0.0 ) THEN
                     C( I ) = ABS( ( A( I ) - T ) / T )
                 ELSE
                     C( I ) = MISSING
                 END IF
101         CONTINUE

        ELSE IF ( DIFMODE .EQ. 11 )  THEN        !  diffnm 2(A - B)/(A + B)

            DO 111  I = 1, NCOL*NROW*NSPC*NLEV
                 S = A( I )
                 T = B( I )
                 U = S + T
                 IF ( U .NE. 0.0 ) THEN
                     C( I ) = 2.0 * ( S - T ) / U
                 ELSE
                     C( I ) = MISSING
                 END IF
111         CONTINUE

        ELSE IF ( DIFMODE .EQ. 12 )  THEN        !  diffnm 2(B - A)/(A + B)

            DO 122  I = 1, NCOL*NROW*NSPC*NLEV
                 S = B( I )
                 T = A( I )
                 U = S + T
                 IF ( U .NE. 0.0 ) THEN
                     C( I ) = 2.0 * ( S - T ) / U
                 ELSE
                     C( I ) = MISSING
                 END IF
122         CONTINUE

        ELSE IF ( DIFMODE .EQ. 13 )  THEN        !  diffnrms

            K = NCOL * NROW
            V = 1.0 / FLOAT( 2 * K )
            DO 133  J = 1, K*NSPC*NLEV, K

                U = 0.0
                DO  130  I = J, J+K-1
                    S = A( I )
                    T = B( I )
                    U = S * S  +  T * T
130             CONTINUE

                IF ( U .NE. 0.0 ) THEN
                    U = SQRT( U*U )
                    DO  131  I = J, J+K-1
                        C( I ) = U * ( A( I ) - B( I ) )
131                 CONTINUE
                ELSE
                    DO  132  I = J, J+K-1
                        C( I ) = MISSING
132                 CONTINUE
                END IF

133         CONTINUE

        ELSE IF ( DIFMODE .EQ. 14 )  THEN        !  diffnrms

            K = NCOL * NROW
            V = 1.0 / FLOAT( 2 * K )
            DO 144  J = 1, K*NSPC*NLEV, K

                U = 0.0
                DO  140  I = J, J+K-1
                    S = A( I )
                    T = B( I )
                    U = S * S  +  T * T
140             CONTINUE

                IF ( U .NE. 0.0 ) THEN
                    U = SQRT( U *U )
                    DO  142  I = J, J+K-1
                        C( I ) = U * ( B( I ) - A( I ) )
142                 CONTINUE
                ELSE
                    DO  143  I = J, J+K-1
                        C( I ) = MISSING
143                 CONTINUE
                END IF

144         CONTINUE

        ELSE IF ( DIFMODE .EQ. 15 )  THEN        !  add

            DO 155  I = 1, NCOL*NROW*NSPC*NLEV
                C( I ) = A( I )  +  B( I )
155         CONTINUE

        ELSE IF ( DIFMODE .EQ. 16 ) THEN         !  max

            DO 166  I = 1, NCOL*NROW*NSPC*NLEV
                C( I ) = MAX( A( I ), B( I ) )
166         CONTINUE

        ELSE IF ( DIFMODE .EQ. 17 ) THEN         !  min

            DO 177  I = 1, NCOL*NROW*NSPC*NLEV
                C( I ) = MIN( A( I ), B( I ) )
177         CONTINUE

        ELSE IF ( DIFMODE .EQ. 18 ) THEN         !  grid A

            DO 188  I = 1, NCOL*NROW*NSPC*NLEV
                C( I ) = A( I )
188         CONTINUE

        ELSE IF ( DIFMODE .EQ. 19 ) THEN         !  grid B

            DO 199  I = 1, NCOL*NROW*NSPC*NLEV
                C( I ) = B( I )
199         CONTINUE

        END IF


        RETURN


C..............................................................................
C............  entry PICKOPS:  user selection of operation via menu choice.
C............  Set DIFMODE and OPNAME accordingly.

        ENTRY  PICKOPS( OPNAME )

        DIFMODE = GETMENU( OPCOUNT , DIFMODE ,
     &                     'Enter number for DATA AGGREGATION MODE',
     &                     DIFMNU )

        OPNAME = OP( DIFMODE )

        RETURN


C..............................................................................
C............ entry NAMEDOP:  select DIFMODE  in terms of OPNAME and then
C............ go to the head of GRIDOPS.

        ENTRY NAMEDOP( OPNAME, NCOL, NROW, NSPC, NLEV, A, B, C )

        I = INDEX1( OPNAME, OPCOUNT, OP )               !  select DIFMODE

        IF ( I .NE. 0 ) THEN

            DIFMODE = I
            GO TO  1            !  to the beginning of GRIDOPS

        ELSE

            CALL M3EXIT( 'GRIDOPS:NAMEDOP', 0,0,
     &                   'Unrecognized grid operation ' // OPNAME, 2 )

        END IF

        RETURN

        END SUBROUTINE GRIDOPS

