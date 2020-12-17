

        SUBROUTINE SPLITLINE( LINE, NMAX, N, FIELD, EFLAG )

        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        !! Version "$Id: splitline.f 1 2017-06-10 18:05:20Z coats $"
        !! Copyright (c) 2014-2015 UNC Institute for the Environment
        !! Distributed under the GNU LESSER PUBLIC LICENSE version 2
        !! See file "LGPL.txt" for conditions of use.
        !!...................................................................
        !  DESCRIPTION:
        !       Split LINE into fields FIELD( N )
        !
        !  REVISION  HISTORY:
        !       Version 1/2001 (?) for I/O API 3.1
        !       Modified 2/2016 for I/O API 3.2:  remove implicit
        !       max-line-length-256 restriction
        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        IMPLICIT NONE

        CHARACTER(LEN=*), INTENT(  IN )::  LINE
        INTEGER,          INTENT(  IN )::  NMAX
        INTEGER,          INTENT( OUT )::  N
        CHARACTER(LEN=*), INTENT( OUT )::  FIELD( NMAX )
        LOGICAL,          INTENT( OUT )::  EFLAG    ! error flag

        !!  PARAMETERS and their descriptions:

        CHARACTER*1,  PARAMETER :: BLANK  = ' '
        CHARACTER*1,  PARAMETER :: TAB    = '	'
        CHARACTER*1,  PARAMETER :: COMMA  = ','
        CHARACTER*1,  PARAMETER :: SEMI   = ';'
        CHARACTER*1,  PARAMETER :: BANG   = '!'
        CHARACTER*1,  PARAMETER :: POUND  = '#'
        CHARACTER*1,  PARAMETER :: DOLLAR = '$'
        CHARACTER*1,  PARAMETER :: QUOTE  = ''''
        CHARACTER*1,  PARAMETER :: QUOTES = '"'


        !!  LOCAL VARIABLES and their descriptions:

        INTEGER         I, J, K, L, M
        CHARACTER*1     CC, DD
        CHARACTER*256   MESG

        !!  STATEMENT FUNCTIONS: separator characters; comment characters

        CHARACTER*1  CH
        LOGICAL      ISSEP, ISCMT

        ISSEP( CH ) = ( ( CH .LE. BLANK ) .OR. ( CH .EQ. TAB  ) .OR.
     &                  ( CH .EQ. COMMA ) .OR. ( CH .EQ. SEMI ) )

        ISCMT( CH ) = ( ( CH .EQ. BANG  ) .OR.
     &                  ( CH .EQ. POUND ) .OR. ( CH .EQ. DOLLAR ) )

        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        EFLAG = .FALSE.
        M     = LEN( LINE )
        J     = 0
        N     = 0

        DO  I = 1, NMAX

            DO K = J+1, 256
                CC = LINE( K:K )
                IF ( .NOT.ISSEP( CC ) ) THEN
                    L = K
                    GO TO  111
                ELSE IF ( ISCMT( CC ) ) THEN
                    RETURN
                END IF
            END DO

            !!  if you get to here:  end of names in list

            EXIT

111             CONTINUE

            !!  so CC = line( L:L ) at this point...

            IF ( CC .EQ.QUOTE ) THEN

                DO K = L+1, M
                    CC = LINE( K:K )
                    IF ( CC .EQ.QUOTE ) THEN
                        N = N + 1           !!  n=I<=NMAX by construction
                        FIELD( N ) = LINE( L+1:K-1 )
                        J = K
                        GO TO  122
                    END IF
                END DO

            ELSE IF ( CC .EQ.QUOTES ) THEN

                DO K = L+1, M
                    CC = LINE( K:K )
                    IF ( CC .EQ.QUOTES ) THEN
                        N = N + 1           !!  n=I<=NMAX by construction
                        FIELD( N ) = LINE( L+1:K-1 )
                        J = K
                        GO TO  122
                    END IF
                END DO

            ELSE

                DO K = L+1, M
                    CC = LINE( K:K )
                    IF ( ISSEP( CC ) ) THEN
                        N = N + 1           !!  n=I<=NMAX by construction
                        FIELD( N ) = LINE( L:K-1 )
                        J = K
                        GO TO  122
                    END IF
                END DO

            END IF

            !!  if you get to here:  error

            WRITE( MESG, '( A, I3, 2X, 3A )' )
     &          'Badly formatted field', N,
     &          'in "', TRIM( LINE ), '"'
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.

122             CONTINUE

        END DO

        DO I = N+1, NMAX
            FIELD( I ) = BLANK
        END DO

        RETURN
        END SUBROUTINE SPLITLINE
