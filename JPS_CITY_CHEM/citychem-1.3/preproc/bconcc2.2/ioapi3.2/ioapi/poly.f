
      REAL    FUNCTION  POLY (XPT, XPTS, YPTS, NDEG)

C***********************************************************************
C Version "$Id: poly.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  64
C
C  FUNCTION:
C        Performs arbitrary-degree polynomial interpolation for XPT on
C        curve determined by XPTS and YPTS using Newton divided-differences.
C  NOTE:  high-order purely polynomial interpolations have stability
C         problems.  NDEG <= 5 is recommended. -- CJC
C
C  REVISION HISTORY:
C
C    11/88   Modified for ROMNET
C    ??/90   Modified for ROM 2.2 by CJC:  scalar coefficient arithmetic --
C            no restrictions on NDEG
C    4/91    Modified by CJC:  optimized initialization
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C        :XPT       point on curve whose value is to be determined
C        :XPTS      points at which function values are known
C                   (there are NDEG + 1 values necessary)
C        :YPTS      function values at each of XPTS
C        :NDEG      degree of polynomial
C
C    Function Value:  POLY      interpolated value at XPT
C
C***********************************************************************

        IMPLICIT NONE

C...........   ARGUMENTS:

        INTEGER, INTENT(IN   ) :: NDEG
        REAL   , INTENT(IN   ) :: XPT
        REAL   , INTENT(IN   ) :: XPTS ( NDEG + 1 )
        REAL   , INTENT(IN   ) :: YPTS ( NDEG + 1 )


C...........   LOCAL VARIABLES:

        INTEGER         I, J, K
        REAL            TDIFF, COEFF
        REAL            DSCR, XSCR, YSCR


C........................................................................
C.......   begin body of POLY

C.......   Compute divided differences: denominator is a product for J <> K.
C.......   Initialization uses unrolled degree I=1 (linear interpolation terms)

        XSCR  =  XPTS ( 1 )
        COEFF =  XPT  -  XSCR
        DSCR  =  COEFF / ( XSCR  -  XPTS( 2 ) )
        YSCR  =  YPTS ( 1 )
        POLY  =  YSCR  +  DSCR * ( YSCR  -  YPTS( 2 ) )


C.......   Now compute higher order terms using divided differences:
C.......   denom is a product for J <> K.

        DO  144  I = 2, NDEG

C.......   Initialization uses unrolled K=1 case.

            XSCR = XPTS ( 1 )
            DSCR = XSCR - XPTS ( 2 )

            DO  100  J = 3 , I + 1
                DSCR = DSCR * ( XSCR - XPTS ( J ) )
100         CONTINUE

            TDIFF  =  YPTS ( 1 ) / DSCR

            DO  133  K = 2, I + 1       !  loop on points K

                XSCR = XPTS ( K )
                DSCR = XSCR - XPTS ( 1 )

                DO  111  J = 2 , K - 1
                    DSCR = DSCR * ( XSCR - XPTS ( J ) )
111             CONTINUE                ! end loop: j not k, part 1

                DO  122  J = K + 1 , I + 1
                    DSCR = DSCR * ( XSCR - XPTS ( J ) )
122             CONTINUE                ! end loop: j not k, part 2


C...........   Compute differences term:

                TDIFF  =  TDIFF  +  YPTS ( K ) / DSCR

133         CONTINUE            !  end loop on points K


C...........   Compute polynomial coefficients:

            COEFF  =  COEFF * ( XPT  -  XPTS ( I ) )

C...........   Compute interpolated value

            POLY = POLY  +  COEFF * TDIFF

144     CONTINUE        !  end loop on terms of degree I


        RETURN
        END FUNCTION POLY

