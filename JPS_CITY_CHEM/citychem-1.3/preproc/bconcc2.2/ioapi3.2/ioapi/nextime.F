
      SUBROUTINE NEXTIME  ( JDATE , JTIME, DTIME )

C.....................................................................
C Version "$Id: nextime.F 88 2018-03-16 17:43:05Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C       subroutine body starts approx line 74
C
C  FUNCTION:
C
C       Subroutine to add  DTIME  to the current time, and then
C       update the clock  JTIME  and calendar  IDATE  accordingly.
C       Output is fully normalized (0 <= JTIME <= 235959).
C
C   IDATE is stored in the form   YYYYDDD = YEAR*1000  +  DAY
C   JTIME is stored in the form   HHMMSS  = HOUR*10000 +  MINS*100  +  SECS
C   DTIME is stored in the form   HHMMSS  = HOUR*10000 +  MINS*100  +  SECS
C
C  CALLS:  none
C
C  REVISION HISTORY:
C       prototype 10/1990 by Carlie J. Coats, Jr., MCNC Environmental Programs
C
C       Version    3/1993 by CJC for CRAY, etc.
C
C       Bugfix     1/2002 by CJC:  stepping _backwards_ across a year
C       transition
C
C       Unification 2/2002 by CJC with global-climate DAYMON, which
C       uses a 360-day "year"
C
C       Unification 2/2002 by CJC with global-climate DAYMON, which
C       uses a 360-day "year"
C
C       Version 5/2005 by CJC:  special case for time-independent data
C       (necessary for date:time=0000000:000000 normalization in CRTFIL3())
C
C       Version 1/2007 by CJC:  handle negative JDATEs correctly
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 11/2015 by CJC: IO_365 changes
C
C       Modified 07/2016 by CJC: Bug-fix in LASTTIME()
C........................................................................

        IMPLICIT  NONE

C.......   ARGUMENTS:

        INTEGER, INTENT(INOUT) :: JDATE           !  date (encoded YYYYDDD)
        INTEGER, INTENT(INOUT) :: JTIME           !  time (encoded  HHMMSS)
        INTEGER, INTENT(IN   ) :: DTIME           !  time increment (encoded HHMMSS)


C.......   LOCAL VARIABLES:  day and year components of date

        INTEGER         IBIAS, IDATE
        INTEGER         YEAR            !  year-component    of IDATE
        INTEGER         DAYS            !  day-component     of IDATE
        INTEGER         HOUR            !  hours-component   of JTIME
        INTEGER         MINS            !  minutes-component of JTIME
        INTEGER         SECS            !  seconds-component of JTIME
        INTEGER         SCR             !  scratch accumulator
        INTEGER         SIGN            !  sign of DTIME
        INTEGER         ATIME           !  absolute value of DTIME

#ifdef IO_360
#define     CLIMODAYS   (360)
#endif

#ifdef IO_365
#define     CLIMODAYS   (365)
#endif


C........................................................................
C       begin  NEXTIME

C.......   Increment seconds part of JTIME by DTIME (secs), and
C.......   re-normalize minute, hour, day part of IDATE:JTIME

        IF ( DTIME .GE. 0 ) THEN
            ATIME = DTIME
            SIGN  = 1
        ELSE
            ATIME = - DTIME
            SIGN  = - 1
        END IF

        SECS  =  MOD ( JTIME , 100 )  +  SIGN * MOD ( ATIME , 100 )
        IF ( SECS .GE. 0 ) THEN
            SCR = SECS / 60
        ELSE
            SCR = - ( 60 - SECS ) / 60
        END IF

        SECS = SECS  -  SCR * 60
        MINS = SCR  +  MOD ( JTIME / 100 , 100 )
     &              +  SIGN * MOD ( ATIME / 100 , 100 )

        IF ( MINS .GE. 0 ) THEN
            SCR = MINS / 60
        ELSE
            SCR = - ( 60 - MINS ) / 60
        END IF

        MINS  =  MINS  -  SCR * 60
        HOUR  =  JTIME / 10000  +  SIGN * ( ATIME / 10000 )  +  SCR

        DAYS  =  0

        IF  ( HOUR .LT. -23 )  THEN
            SCR  = -HOUR / 24
            HOUR =  HOUR  +  SCR * 24
            DAYS =  DAYS  -  SCR
        END IF

        IF  ( HOUR .LT. 0 )  THEN

            SCR   =  ( 24 - HOUR ) / 24
            HOUR  =  HOUR  +  SCR * 24
            DAYS  =  DAYS  -  SCR

        ELSE IF  ( HOUR .GT. 23 )  THEN

            SCR   =  HOUR / 24
            HOUR  =  HOUR  -  SCR * 24
            DAYS  =  DAYS  +  SCR

        END IF

        JTIME  =  10000 * HOUR  +  100 * MINS  +  SECS

        IF ( DAYS .EQ. 0 .AND. JDATE .EQ. 0 ) THEN
            RETURN
        END IF

C.......   Construct equivalent problem with positive IDATE = JDATE + IBIAS

        IF ( JDATE .GE. 2000 ) THEN
            IDATE = JDATE
            IBIAS = 0
        ELSE IF ( JDATE .GE. -2000 ) THEN       !! standard-year processing
            IDATE = JDATE + 3000
            DAYS  = DAYS + MOD ( IDATE , 1000 )
            YEAR  = IDATE / 1000 - 3
            JDATE = 1000 * YEAR  +  DAYS
            RETURN
        ELSE    ! adjust date by multiple of 400-year leap-year cycle
            YEAR  = ABS( JDATE )
            YEAR  = YEAR / 1000 + 1
            YEAR  = 400 * ( YEAR / 400 + 1 )
            IBIAS = 1000 * YEAR
            IDATE = JDATE + IBIAS
        END IF

C...........   Update IDATE:
C...........   Note that each year must be treated individually

        DAYS  =  DAYS + MOD ( IDATE , 1000 )
        YEAR  =  IDATE / 1000

100     CONTINUE        !  loop normalizing negative day numbers

            IF ( DAYS .LE. 0 ) THEN

#ifdef CLIMODAYS
                DAYS  =  CLIMODAYS   +  DAYS
                YEAR  =  YEAR  -  1
#endif

#ifndef CLIMODAYS

                IF (           ( MOD (YEAR,4)   .NE. 1 )        !  nonleap year
     &             .OR. (      ( MOD (YEAR,100) .EQ. 1 )
     &                   .AND. ( MOD (YEAR,400) .NE. 1 ) ) ) THEN

                    DAYS  =  365   +  DAYS
                    YEAR  =  YEAR  -  1

                ELSE            !  leap-year case

                    DAYS  =  366   +  DAYS
                    YEAR  =  YEAR  -  1

                END IF

#endif

                GO TO  100

            END IF


200     CONTINUE        !  loop normalizing day numbers > 365,366

#ifdef CLIMODAYS

            IF ( DAYS .GE. CLIMODAYS+1 ) THEN

                DAYS  =  DAYS  - CLIMODAYS
                YEAR  =  YEAR  +   1

                GO TO  200

            END IF      !  end DAYS > 365,366 date-normalization loop

#endif

#ifndef CLIMODAYS

            IF ( DAYS .GE. 366 ) THEN

                IF (           ( MOD (YEAR,4)   .NE. 0 )        !  nonleap year
     &              .OR. (     ( MOD (YEAR,100) .EQ. 0 )
     &                   .AND. ( MOD (YEAR,400) .NE. 0 ) ) ) THEN

                    DAYS  =  DAYS   - 365
                    YEAR  =  YEAR  +   1

                    GO TO  200

                ELSE IF ( DAYS .GE. 367 ) THEN           !  leap year case

                    DAYS  =  DAYS   - 366
                    YEAR  =  YEAR  +   1

                    GO TO  200

                END IF

            END IF      !  end DAYS > 365,366 date-normalization loop

#endif

        JDATE  =   1000 * YEAR  +  DAYS - IBIAS
        IF ( JDATE .EQ. -635 )  JDATE = 0       !!  for standard-week/month/year accounting

        RETURN
        END  SUBROUTINE NEXTIME



        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        SUBROUTINE LASTTIME( SDATE,STIME,TSTEP,NRECS, EDATE,ETIME )

        IMPLICIT  NONE

C.......   ARGUMENTS:

        INTEGER, INTENT(IN   ) :: SDATE, STIME, TSTEP, NRECS
        INTEGER, INTENT(  OUT) :: EDATE, ETIME

C.......   PARAMETERs:

        INTEGER, PARAMETER :: S365 = 365 * 24 * 60 * 60     !!  seconds
        INTEGER, PARAMETER :: T365 = 365 * 24 * 10000       !!  time as H*MMSS

        !!.......   EXTERNAL Functions:

        INTEGER, EXTERNAL :: SEC2TIME, TIME2SEC

        !!.......   LOCAL VARIABLES:  day and year components of date

        INTEGER     ISEC, STEP
        INTEGER*8   IREC, SECS

        !!  Normalized copies of the arguments:

        EDATE = SDATE
        ETIME = STIME
        CALL NEXTIME( EDATE, ETIME, 0 )

        IF ( TSTEP .EQ. 0 ) THEN   !  time-independent case:
            RETURN
        ELSE IF ( NRECS .LE. 1 ) THEN   !  at most 1 record
            RETURN
        END  IF

        IREC = NRECS - 1
        SECS = IREC * TIME2SEC( ABS( TSTEP ) )

        DO
            IF ( SECS .LT. S365 ) EXIT
            SECS = SECS - S365
            CALL NEXTIME( EDATE, ETIME, T365 )
        END DO

        ISEC = SECS
        STEP = SEC2TIME( ISEC )
        CALL NEXTIME( EDATE, ETIME, STEP )

        RETURN

        END SUBROUTINE LASTTIME

