
        INTEGER  FUNCTION GCD ( P , Q )

C***********************************************************************
C Version "$Id: gcd.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  GCD function body starts at line 44
C  LCM function body starts at line 77
C
C  FUNCTION:
C       GCD computes greatest common divisors, and LCM computes
C       the least common multiple of integers P,Q
C
C  PRECONDITIONS REQUIRED:  none
C
C  REVISION  HISTORY:
C       prototype 3/1991 by CJC
C
C       Bugfix    9/2004 by CJC: handle case that P=0 or Q=0
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C***********************************************************************

        IMPLICIT NONE

C...........   Arguments:

        INTEGER, INTENT(IN   ) :: P , Q


C.......   Local Variables:

        INTEGER 	X, Y


C***********************************************************************
C   begin body of program  GCD2

        IF ( P .EQ. 0 ) THEN
            GCD = 0
            RETURN
        ELSE IF ( Q .EQ. 0 ) THEN
            GCD = 0
            RETURN
        END IF

        X = ABS ( P )
        Y = ABS ( Q )

111     CONTINUE

            IF ( X .GT. Y ) THEN
                X = MOD ( X , Y )
                IF ( X .NE. 0 ) GO TO 111
            ELSE IF ( X .LT. Y ) THEN
                Y = MOD ( Y , X )
                IF ( Y .NE. 0 ) GO TO 111
            END IF

        GCD = MAX ( X , Y )

        RETURN

        END FUNCTION GCD



        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



        INTEGER FUNCTION LCM( I, J )
            INTEGER, INTENT( IN ) :: I, J
            INTEGER, EXTERNAL     :: GCD
            LCM = ( I * J ) / GCD( I, J )
        END FUNCTION LCM
