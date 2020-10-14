
        MODULE M3UTILIO

        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        !! Version "$Id: m3utilio.f 107 2018-07-26 14:05:39Z coats $"
        !! Copyright (c) 2004-2013 Baron Advanced Meteorological Systems,
        !! (c) 2007-2013 Carlie J. Coats, Jr., and
        !! (C) 2014 UNC Institute for the Environment.
        !! Distributed under the GNU LESSER PUBLIC LICENSE version 2.1
        !! See file "LGPL.txt" for conditions of use.
        !!...................................................................
        !!  DESCRIPTION:
        !!      Models-3 I/O API declarations and INTERFACE blocks.
        !!      Additional utility routines:
        !!          SPLITLINE: Split LINE into fields FIELD( N )
        !!          FIXFIELD:  Convert "missing" = "-9" fields and
        !!                     leading blanks in FIELD to all-zeros
        !!          KEYVAL:    retrieve value of REAL KEY from FDESC3D fields
        !!          KEYSTR:    retrieve value of char-string KEY.
        !!
        !!  DO NOT EDIT !!!!
        !!
        !!        The EDSS/Models-3 I/O API depends in an essential manner
        !!        upon the contents of this MODULE file.  ANY CHANGES are
        !!        likely to result in very obscure, difficult-to-diagnose
        !!        bugs caused by an inconsistency between standard "libioapi.a"
        !!        object-libraries and whatever code is compiled with the
        !!        resulting modified MODULE-file.
        !!
        !!        By making any changes to this MODULE file, the user
        !!        explicitly agrees that in the case any assistance is
        !!        required of MCNC or of the I/O API author, Carlie J. Coats, Jr.
        !!        THE USER AND/OR HIS PROJECT OR CONTRACT AGREES TO REIMBURSE
        !!        UNC AND/OR THE I/O API AUTHOR, CARLIE J. COATS, JR., AT A
        !!        RATE TRIPLE THE NORMAL CONTRACT RATE FOR THE SERVICES
        !!        REQUIRED.
        !!
        !!  PRECONDITIONS:
        !!      Consistency of INTERFACE blocks with I/O API sources.
        !!
        !!  REVISION  HISTORY:
        !!      Prototype 11/2004 by Carlie J. Coats, Jr., BAMS,
        !!      for WRF/sub-grid SMOKE development.
        !!      Version  01/2013:  new routine LASTTIME(); consistency with v3.1
        !!      Version  03/2014:  Generic (INTEGER(4 | 8) SORTIC().
        !!      Generic ENVLIST(), FINDKEY(), LOCATE(), SORTI()
        !!      Version  05/2014:  bugfix in SORTI()
        !!      Version  06/2014:  Add SORTIN4(), SORTIN8() interfaces to SORTI()
        !!      Version  08/2014:  Add FINDL1() ... FINDL4, LOCATL1() ... LOCATL4(),
        !!      SORTL1() ...SORTL4() interfaces to FINDKEY(), LOCAT(), SORTI()
        !!      Version  07/2015:  move LASTTIME() into "nexttime.F", and
        !!      provide INTERFACE for it.
        !!      Version  11/2015:  re-add LAMBERT etc. INTERFACEs from 3.1,
        !!      together with re-naming clauses for MODULE MODGCTP
        !!      Version  07/2018:  Add INDEXL1.  Generic INDEXKEY.
        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            IMPLICIT NONE

            INCLUDE 'PARMS3.EXT'        !  I/O API PARAMETERs
            INCLUDE 'FDESC3.EXT'        !  I/O API file headers
            INCLUDE 'IODECL3.EXT'       !  I/O API function declarations

            CHARACTER*72, PRIVATE, SAVE :: ID =
     &'$Id:: m3utilio.f 107 2018-07-26 14:05:39Z coats                $'


            !!........  PUBLIC Routines:

            PUBLIC  FIXFIELD, KEYVAL, KEYSTR, LCM


            !!........  INTERFACE Blocks:
            !!
            !!    BILIN, BMATVEC, CURREC, DAYMON, DMATVEC, DT2STR, ENVDBLE,
            !!    ENVLIST, ENVGET, (ENVINT, ENVREAL, ENVSTR, ENVYN...), FINDKEY, FINDC,
            !!    FIND1, FIND2, FIND3, FIND4, FINDL1, FINDL2, FINDL3, FINDL4,
            !!    FINDR1, FINDR2, FINDR3, FINDR4,
            !!    GCD, GETDATE, GETDBLE, GETDFILE, GETEFILE, GETFFILE,
            !!    GETMENU, GETNUM, GETREAL, GETSTR, GETDTTIME, GETYN, HHMMSS,
            !!    INDEX1, INDEXINT1, INTLIST, ISDST, JSTEP3, JULIAN, LBLANK,
            !!    LOCATE, LOCAT1, LOCAT2, LOCAT3, LOCAT4, LOCATC,
            !!    LOCATL1, LOCATL2, LOCATL3, LOCATR4, LOCATL1, LOCATR2, LOCATR3, LOCATR4,
            !!    M3EXIT, M3FLUSH, M3MESG, M3MSG2, M3PARAG,
            !!    M3PROMPT, M3WARN, MMDDYY, NEXTIME, PMATVEC, POLY,
            !!    PROMPTDFILE, PROMPTFFILE, PROMPTMFILE, REALIST, SETENVVAR,
            !!    SORTIC, SORTI, SORTI1, SORTI2, SORTI3, SORTI4,
            !!    SORTL1, SORTL2, SORTL3, SORTL4, SORTR1, SORTR2, SORTR3, SORTR4,
            !!    SPLITLINE, STR2DBLE,
            !!    STR2INT, STR2REAL, STRLIST, SEC2TIME, SECSDIFF,
            !!    TIME2SEC, UNGRIDB, UNGRIDI, WKDAY, YEAR4, YR2DAY

            INTERFACE BILIN

                SUBROUTINE  BILIN11L( M, N, P, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: M               ! length of input  vector
                INTEGER, INTENT(IN   ) :: N               ! length of output vector
                INTEGER, INTENT(IN   ) :: P               ! number of layers
                INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
                REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( M,P )        ! P-layered input  vector
                REAL   , INTENT(  OUT) :: Y( N,P )        ! P-layered output vector
                END SUBROUTINE  BILIN11L

                SUBROUTINE  BILIN12L( M, NC, NR, P, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: M               ! length of input  vector
                INTEGER, INTENT(IN   ) :: NC, NR            ! dims of output grid
                INTEGER, INTENT(IN   ) :: P               ! number of layers
                INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )   ! index array
                REAL   , INTENT(IN   ) :: AX( 4,NC*NR )   ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( M,P )        ! P-layered input  vector
                REAL   , INTENT(  OUT) :: Y( NC,NR,P )    ! P-layered output vector
                END SUBROUTINE  BILIN12L

                SUBROUTINE  BILIN21L( MC, MR, N, P, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: MC, MR          ! length of input  vector
                INTEGER, INTENT(IN   ) :: N                 ! dims of output grid
                INTEGER, INTENT(IN   ) :: P               ! number of layers
                INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
                REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( MC,MR,P )    ! P-layered input  vector
                REAL   , INTENT(  OUT) :: Y( N,P )        ! P-layered output vector
                END SUBROUTINE  BILIN21L

                SUBROUTINE  BILIN22L( MC, MR, NC, NR, P, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: MC, MR          ! length of input  grid
                INTEGER, INTENT(IN   ) :: NC, NR            ! dims of output grid
                INTEGER, INTENT(IN   ) :: P               ! number of layers
                INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )   ! index array
                REAL   , INTENT(IN   ) :: AX( 4,NC*NR )   ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( MC,MR,P )    ! P-layered input  vector
                REAL   , INTENT(  OUT) :: Y( NC,NR,P )    ! P-layered output vector
                END SUBROUTINE  BILIN22L

                SUBROUTINE  BILIN11( M, N, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: M               ! length of input  vector
                INTEGER, INTENT(IN   ) :: N               ! length of output vector
                INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
                REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( M )          ! nonlayered input  vector
                REAL   , INTENT(  OUT) :: Y( N )          ! nonlayered output vector
                END SUBROUTINE  BILIN11

                SUBROUTINE  BILIN12( M, NC, NR, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: M               ! length of input  vector
                INTEGER, INTENT(IN   ) :: NC, NR            ! dims of output grid
                INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )   ! index array
                REAL   , INTENT(IN   ) :: AX( 4,NC*NR )   ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( M )          ! nonlayered input  vector
                REAL   , INTENT(  OUT) :: Y( NC,NR )      ! nonlayered output vector
                END SUBROUTINE  BILIN12

                SUBROUTINE  BILIN21( MC, MR, N, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: MC, MR          ! length of input  vector
                INTEGER, INTENT(IN   ) :: N               ! dims of output grid
                INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
                REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( MC,MR )      ! nonlayered input  vector
                REAL   , INTENT(  OUT) :: Y( N )          ! nonlayered output vector
                END SUBROUTINE  BILIN21

                SUBROUTINE  BILIN22( MC, MR, NC, NR, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: MC, MR          ! length of input  grid
                INTEGER, INTENT(IN   ) :: NC, NR            ! dims of output grid
                INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )   ! index array
                REAL   , INTENT(IN   ) :: AX( 4,NC*NR )   ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( MC,MR )      ! nonlayered input  vector
                REAL   , INTENT(  OUT) :: Y( NC,NR )      ! nonlayered output vector
                END SUBROUTINE  BILIN22

            END INTERFACE       !!  BILIN

            INTERFACE BMATVEC

                !!  non-layered cases:

                SUBROUTINE  BMATVEC01( M, N, P, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: M               ! length of input  vector
                INTEGER, INTENT(IN   ) :: N               ! length of output vector
                INTEGER, INTENT(IN   ) :: P               ! number of layers
                INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
                REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( M )          ! P-layered input  vector
                REAL   , INTENT(  OUT) :: Y( N )          ! P-layered output vector
                END SUBROUTINE  BMATVEC01

                SUBROUTINE  BMATVEC02( M, NC, NR, P, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: M               ! length of input  vector
                INTEGER, INTENT(IN   ) :: NC, NR          ! length of output vector
                INTEGER, INTENT(IN   ) :: P               ! number of layers
                INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )   ! index array
                REAL   , INTENT(IN   ) :: AX( 4,NC*NR )   ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( M )          ! P-layered input  vector
                REAL   , INTENT(  OUT) :: Y( NC,NR )      ! P-layered output vector
                END SUBROUTINE  BMATVEC02

                SUBROUTINE  BMATVEC021( MC, MR, N, P, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: MC, MR          ! length of input  vector
                INTEGER, INTENT(IN   ) :: N               ! length of output vector
                INTEGER, INTENT(IN   ) :: P               ! number of layers
                INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
                REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( MC,MR )      ! P-layered input  vector
                REAL   , INTENT(  OUT) :: Y( N )          ! P-layered output vector
                END SUBROUTINE  BMATVEC021

                SUBROUTINE  BMATVEC022( MC,MR, NC,NR, P, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: MC, MR          ! length of input  vector
                INTEGER, INTENT(IN   ) :: NC, NR          ! length of output vector
                INTEGER, INTENT(IN   ) :: P               ! number of layers
                INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )   ! index array
                REAL   , INTENT(IN   ) :: AX( 4,NC*NR )   ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( MC,MR )      ! P-layered input  vector
                REAL   , INTENT(  OUT) :: Y( NC,NR )      ! P-layered output vector
                END SUBROUTINE  BMATVEC022

                !!  layered cases:

                SUBROUTINE  BMATVEC11( M, N, P, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: M               ! length of input  vector
                INTEGER, INTENT(IN   ) :: N               ! length of output vector
                INTEGER, INTENT(IN   ) :: P               ! number of layers
                INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
                REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( M,P )        ! P-layered input  vector
                REAL   , INTENT(  OUT) :: Y( P,N )        ! P-layered output vector
                END SUBROUTINE  BMATVEC11

                SUBROUTINE  BMATVEC12( M, NC, NR, P, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: M               ! length of input  vector
                INTEGER, INTENT(IN   ) :: NC, NR          ! length of output vector
                INTEGER, INTENT(IN   ) :: P               ! number of layers
                INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )   ! index array
                REAL   , INTENT(IN   ) :: AX( 4,NC*NR )   ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( M,P )        ! P-layered input  vector
                REAL   , INTENT(  OUT) :: Y( P,NC,NR )    ! P-layered output vector
                END SUBROUTINE  BMATVEC12

                SUBROUTINE  BMATVEC21( MC, MR, N, P, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: MC, MR          ! length of input  vector
                INTEGER, INTENT(IN   ) :: N               ! length of output vector
                INTEGER, INTENT(IN   ) :: P               ! number of layers
                INTEGER, INTENT(IN   ) :: IX( 4,N )       ! index array
                REAL   , INTENT(IN   ) :: AX( 4,N )       ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( MC,MR,P )    ! P-layered input  vector
                REAL   , INTENT(  OUT) :: Y( P,N )        ! P-layered output vector
                END SUBROUTINE  BMATVEC21

                SUBROUTINE  BMATVEC22( MC, MR, NC, NR, P, IX, AX, V, Y )
                INTEGER, INTENT(IN   ) :: MC, MR          ! length of input  vector
                INTEGER, INTENT(IN   ) :: NC, NR          ! length of output vector
                INTEGER, INTENT(IN   ) :: P               ! number of layers
                INTEGER, INTENT(IN   ) :: IX( 4,NC*NR )   ! index array
                REAL   , INTENT(IN   ) :: AX( 4,NC*NR )   ! 4-band coeff matrix
                REAL   , INTENT(IN   ) :: V( MC,MR,P )    ! P-layered input  vector
                REAL   , INTENT(  OUT) :: Y( P,NC,NR )    ! P-layered output vector
                END SUBROUTINE  BMATVEC22

            END INTERFACE       !!  BMATVEC

            INTERFACE
                LOGICAL FUNCTION CHKBUF3( FDUM )
                INTEGER, INTENT(  OUT) :: FDUM            !  prevents excessive optimization
                END FUNCTION CHKBUF3
            END INTERFACE

            INTERFACE
                CHARACTER*2 FUNCTION CRLF()
                END FUNCTION CRLF
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION CURREC ( JDATE, JTIME,
     &                                    SDATE, STIME, TSTEP,
     &                                    CDATE, CTIME )
                INTEGER, INTENT(IN   ) :: SDATE, STIME    !  starting d&t for the sequence
                INTEGER, INTENT(IN   ) :: TSTEP           !  time step for the sequence
                INTEGER, INTENT(IN   ) :: JDATE, JTIME    !  d&t requested
                INTEGER, INTENT(  OUT) :: CDATE, CTIME    !  d&t for timestep of JDATE:JTIME
                END FUNCTION CURREC
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION CURRSTEP ( JDATE, JTIME,
     &                                      SDATE, STIME, TSTEP,
     &                                      CDATE, CTIME )
                INTEGER, INTENT(IN   ) :: SDATE, STIME    !  starting d&t for the sequence
                INTEGER, INTENT(IN   ) :: TSTEP           !  time step for the sequence
                INTEGER, INTENT(IN   ) :: JDATE, JTIME    !  d&t requested
                INTEGER, INTENT(  OUT) :: CDATE, CTIME    !  d&t for timestep of JDATE:JTIME
                END FUNCTION CURRSTEP
            END INTERFACE

            INTERFACE
                SUBROUTINE DAYMON( JDATE, MNTH, MDAY )
                INTEGER, INTENT(IN   ) :: JDATE     !  Julian date, format YYYYDDD = 1000*Year + Day
                INTEGER, INTENT(  OUT) :: MNTH    !  month (1...12)
                INTEGER, INTENT(  OUT) :: MDAY    !  day-of-month (1...28,29,30,31)
                END SUBROUTINE  DAYMON
            END INTERFACE

            INTERFACE
                SUBROUTINE  DMATVEC( N, A, V, C )
                INTEGER, INTENT(IN   ) :: N             ! length of input vector
                REAL   , INTENT(IN   ) :: A( N )		! diagonal coeff matrix
                REAL   , INTENT(IN   ) :: V( N )		! input  vector
                REAL   , INTENT(  OUT) :: C( N )		! output vector
                END SUBROUTINE  DMATVEC
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION  DSCGRID( GNAME, CNAME,
     &              CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &              XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK )
                CHARACTER*(*), INTENT(IN   ) :: GNAME	!  grid  sys name
                CHARACTER*(*), INTENT(  OUT) :: CNAME	!  coord sys name
                INTEGER,       INTENT(  OUT) :: CTYPE	!  coord sys type
                REAL*8 ,       INTENT(  OUT) :: P_ALP	!  first, second, third map
                REAL*8 ,       INTENT(  OUT) :: P_BET	!  projection descriptive
                REAL*8 ,       INTENT(  OUT) :: P_GAM	!  parameters
                REAL*8 ,       INTENT(  OUT) :: XCENT	!  lon for coord-system X=0
                REAL*8 ,       INTENT(  OUT) :: YCENT	!  lat for coord-system Y=0
                REAL*8 ,       INTENT(  OUT) :: XORIG	!  X-coordinate origin of grid (map units)
                REAL*8 ,       INTENT(  OUT) :: YORIG	!  Y-coordinate origin of grid
                REAL*8 ,       INTENT(  OUT) :: XCELL	!  X-coordinate cell dimension
                REAL*8 ,       INTENT(  OUT) :: YCELL	!  Y-coordinate cell dimension
                INTEGER,       INTENT(  OUT) :: NCOLS	!  number of grid columns
                INTEGER,       INTENT(  OUT) :: NROWS	!  number of grid rows
                INTEGER,       INTENT(  OUT) :: NTHIK	!  BOUNDARY:  perimeter thickness (cells)
                END FUNCTION  DSCGRID
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION DSCOORD( CNAME, CTYPE,
     &                                    P_ALP, P_BET, P_GAM,
     &                                    XCENT, YCENT )
                CHARACTER*(*), INTENT(IN   ) :: CNAME	!  coord sys name
                INTEGER      , INTENT(  OUT) :: CTYPE	!  coord sys type
                REAL*8       , INTENT(  OUT) :: P_ALP	!  first, second, third map
                REAL*8       , INTENT(  OUT) :: P_BET	!  projection descriptive
                REAL*8       , INTENT(  OUT) :: P_GAM	!  parameters
                REAL*8       , INTENT(  OUT) :: XCENT	!  lon for coord-system X=0
                REAL*8       , INTENT(  OUT) :: YCENT	!  lat for coord-system Y=0
                END FUNCTION DSCOORD
            END INTERFACE

            INTERFACE
                CHARACTER(LEN=24) FUNCTION  DT2STR( JDATE , JTIME )
                INTEGER, INTENT(IN   ) :: JDATE   !  Julian date, coded YYYYDDD
                INTEGER, INTENT(IN   ) :: JTIME   !  time, coded HHMMSS
                END FUNCTION  DT2STR
            END INTERFACE

            INTERFACE ENVLIST

                LOGICAL FUNCTION DBLLIST( ENAME, EDESC,
     &                                    NMAX, NCNT, LIST )
                CHARACTER*(*), INTENT(IN   ) :: ENAME   ! environment variable for the list
                CHARACTER*(*), INTENT(IN   ) :: EDESC   ! environment variable description
                INTEGER      , INTENT(IN   ) :: NMAX    ! dimension for list
                INTEGER      , INTENT(  OUT) :: NCNT    ! actual number of entries in list
                REAL*8       , INTENT(  OUT) :: LIST( NMAX )    ! array of values found
                END FUNCTION DBLLIST


                LOGICAL FUNCTION INTLIST( ENAME, EDESC,
     &                                    NMAX, NCNT, LIST )
                CHARACTER*(*), INTENT(IN   ) :: ENAME   !  environment variable for the list
                CHARACTER*(*), INTENT(IN   ) :: EDESC   !  environment variable description
                INTEGER      , INTENT(IN   ) :: NMAX    !  dimension for list
                INTEGER      , INTENT(  OUT) :: NCNT    !  actual number of entries in list
                INTEGER      , INTENT(  OUT) :: LIST( NMAX )    ! array of values found
                END FUNCTION INTLIST

                LOGICAL FUNCTION REALIST( ENAME, EDESC,
     &                                    NMAX, NCNT, LIST )
                CHARACTER*(*), INTENT(IN   ) :: ENAME   ! environment variable for the list
                CHARACTER*(*), INTENT(IN   ) :: EDESC   ! environment variable description
                INTEGER      , INTENT(IN   ) :: NMAX    ! dimension for list
                INTEGER      , INTENT(  OUT) :: NCNT    ! actual number of entries in list
                REAL         , INTENT(  OUT) :: LIST( NMAX )    ! array of values found
                END FUNCTION REALIST

                LOGICAL FUNCTION STRLIST( ENAME, EDESC,
     &                                    NMAX, NCNT, LIST )
                CHARACTER*(*), INTENT(IN   ) :: ENAME           !  environment variable for the list
                CHARACTER*(*), INTENT(IN   ) :: EDESC           !  environment variable description
                INTEGER      , INTENT(IN   ) :: NMAX            !  dimension for list
                INTEGER      , INTENT(  OUT) :: NCNT            !  actual number of entries in list
                CHARACTER*(*), INTENT(  OUT) :: LIST( NMAX )    ! array of values found
                END FUNCTION STRLIST

            END INTERFACE       !!  envlist()


            INTERFACE ENVGET

                REAL*8 FUNCTION BENVDBLE( LNAME, DESC, 
     &                                    LO, HI, DEFAULT, STAT )
                CHARACTER*(*), INTENT(IN   ) :: LNAME
                CHARACTER*(*), INTENT(IN   ) :: DESC
                REAL*8       , INTENT(IN   ) :: LO
                REAL*8       , INTENT(IN   ) :: HI
                REAL*8       , INTENT(IN   ) :: DEFAULT
                INTEGER      , INTENT(  OUT) :: STAT
                END FUNCTION BENVDBLE

                INTEGER FUNCTION BENVINT( LNAME, DESC, 
     &                                    LO, HI, DEFAULT, STAT )
                CHARACTER*(*), INTENT(IN   ) :: LNAME
                CHARACTER*(*), INTENT(IN   ) :: DESC
                INTEGER      , INTENT(IN   ) :: LO
                INTEGER      , INTENT(IN   ) :: HI
                INTEGER      , INTENT(IN   ) :: DEFAULT
                INTEGER      , INTENT(  OUT) :: STAT
                END FUNCTION BENVINT

                REAL FUNCTION BENVREAL( LNAME, DESC, 
     &                                    LO, HI, DEFAULT, STAT )
                CHARACTER*(*), INTENT(IN   ) :: LNAME
                CHARACTER*(*), INTENT(IN   ) :: DESC
                REAL         , INTENT(IN   ) :: LO
                REAL         , INTENT(IN   ) :: HI
                REAL         , INTENT(IN   ) :: DEFAULT
                INTEGER      , INTENT(  OUT) :: STAT
                END FUNCTION BENVREAL

                REAL*8 FUNCTION ENVDBLE( LNAME, DESC, DEFAULT, STAT )
                CHARACTER*(*), INTENT(IN   ) :: LNAME
                CHARACTER*(*), INTENT(IN   ) :: DESC
                REAL*8       , INTENT(IN   ) :: DEFAULT
                INTEGER      , INTENT(  OUT) :: STAT
                END FUNCTION ENVDBLE

                INTEGER FUNCTION ENVINT( LNAME, DESC, DEFAULT, STAT )
                CHARACTER*(*), INTENT(IN   ) :: LNAME
                CHARACTER*(*), INTENT(IN   ) :: DESC
                INTEGER      , INTENT(IN   ) :: DEFAULT
                INTEGER      , INTENT(  OUT) :: STAT
                END FUNCTION ENVINT

                REAL FUNCTION ENVREAL( LNAME, DESC, DEFAULT, STAT )
                CHARACTER*(*), INTENT(IN   ) :: LNAME
                CHARACTER*(*), INTENT(IN   ) :: DESC
                REAL         , INTENT(IN   ) :: DEFAULT
                INTEGER      , INTENT(  OUT) :: STAT
                END FUNCTION ENVREAL

                LOGICAL FUNCTION ENVYN( LNAME, DESC, DEFAULT, STAT )
                CHARACTER*(*), INTENT(IN   ) :: LNAME
                CHARACTER*(*), INTENT(IN   ) :: DESC
                LOGICAL      , INTENT(IN   ) :: DEFAULT
                INTEGER      , INTENT(  OUT) :: STAT
                END FUNCTION ENVYN

            END INTERFACE           !! envget()


            INTERFACE

                SUBROUTINE ENVSTR( LNAME, DESC, DEFAULT, EQNAME, STAT )
                CHARACTER*(*), INTENT(IN   ) :: LNAME
                CHARACTER*(*), INTENT(IN   ) :: DESC
                CHARACTER*(*), INTENT(IN   ) :: DEFAULT
                CHARACTER*(*), INTENT(  OUT) :: EQNAME
                INTEGER      , INTENT(  OUT) :: STAT
                END SUBROUTINE ENVSTR

            END INTERFACE           !! envstr()


            INTERFACE FINDKEY

                INTEGER FUNCTION FINDC( KEY, N, LIST )
                CHARACTER*(*), INTENT(IN   ) :: KEY           !  key
                INTEGER      , INTENT(IN   ) :: N             !  table size
                CHARACTER*(*), INTENT(IN   ) :: LIST( N )     !  table to search for KEY
                END FUNCTION FINDC

                INTEGER FUNCTION FIND1( K, N, LIST )
                INTEGER, INTENT(IN   ) :: K             !  first  key
                INTEGER, INTENT(IN   ) :: N             !  table size
                INTEGER, INTENT(IN   ) :: LIST( N )     !  table to search for K
                END FUNCTION FIND1

                INTEGER FUNCTION FIND2( K1, K2, N, LIST1, LIST2 )
                INTEGER, INTENT(IN   ) :: K1             !  first  key
                INTEGER, INTENT(IN   ) :: K2             !  second key
                INTEGER, INTENT(IN   ) :: N              !  table size
                INTEGER, INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                INTEGER, INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                END FUNCTION FIND2

                INTEGER FUNCTION FIND3( K1, K2, K3,
     &                                  N, LIST1, LIST2, LIST3 )
                INTEGER, INTENT(IN   ) :: K1             !  first  key
                INTEGER, INTENT(IN   ) :: K2             !  second key
                INTEGER, INTENT(IN   ) :: K3             !  third  key
                INTEGER, INTENT(IN   ) :: N              !  table size
                INTEGER, INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                INTEGER, INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                INTEGER, INTENT(IN   ) :: LIST3( N )     !  table to search for K3
                END FUNCTION FIND3

                INTEGER FUNCTION FIND4( K1, K2, K3, K4,
     &                                  N, LIST1, LIST2, LIST3, LIST4 )
                INTEGER, INTENT(IN   ) :: K1             !  first  key
                INTEGER, INTENT(IN   ) :: K2             !  second key
                INTEGER, INTENT(IN   ) :: K3             !  third  key
                INTEGER, INTENT(IN   ) :: K4             !  third  key
                INTEGER, INTENT(IN   ) :: N              !  table size
                INTEGER, INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                INTEGER, INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                INTEGER, INTENT(IN   ) :: LIST3( N )     !  table to search for K3
                INTEGER, INTENT(IN   ) :: LIST4( N )     !  table to search for K4
                END FUNCTION FIND4

                INTEGER FUNCTION FINDL1( K, N, LIST )
                INTEGER(8), INTENT(IN   ) :: K             !  first  key
                INTEGER   , INTENT(IN   ) :: N             !  table size
                INTEGER(8), INTENT(IN   ) :: LIST( N )     !  table to search for K
                END FUNCTION FINDL1

                INTEGER(8) FUNCTION FINDL2( K1, K2, N, LIST1, LIST2 )
                INTEGER(8), INTENT(IN   ) :: K1             !  first  key
                INTEGER(8), INTENT(IN   ) :: K2             !  second key
                INTEGER   , INTENT(IN   ) :: N              !  table size
                INTEGER(8), INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                INTEGER(8), INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                END FUNCTION FINDL2

                INTEGER(8) FUNCTION FINDL3( K1, K2, K3,
     &                                  N, LIST1, LIST2, LIST3 )
                INTEGER(8), INTENT(IN   ) :: K1             !  first  key
                INTEGER(8), INTENT(IN   ) :: K2             !  second key
                INTEGER(8), INTENT(IN   ) :: K3             !  third  key
                INTEGER   , INTENT(IN   ) :: N              !  table size
                INTEGER(8), INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                INTEGER(8), INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                INTEGER(8), INTENT(IN   ) :: LIST3( N )     !  table to search for K3
                END FUNCTION FINDL3

                INTEGER(8) FUNCTION FINDL4( K1, K2, K3, K4,
     &                                  N, LIST1, LIST2, LIST3, LIST4 )
                INTEGER(8), INTENT(IN   ) :: K1             !  first  key
                INTEGER(8), INTENT(IN   ) :: K2             !  second key
                INTEGER(8), INTENT(IN   ) :: K3             !  third  key
                INTEGER(8), INTENT(IN   ) :: K4             !  third  key
                INTEGER   , INTENT(IN   ) :: N              !  table size
                INTEGER(8), INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                INTEGER(8), INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                INTEGER(8), INTENT(IN   ) :: LIST3( N )     !  table to search for K3
                INTEGER(8), INTENT(IN   ) :: LIST4( N )     !  table to search for K4
                END FUNCTION FINDL4

                INTEGER FUNCTION FINDR1( K, N, LIST )
                REAL   , INTENT(IN   ) :: K             !  first  key
                INTEGER, INTENT(IN   ) :: N             !  table size
                REAL   , INTENT(IN   ) :: LIST( N )     !  table to search for K
                END FUNCTION FINDR1

                INTEGER FUNCTION FINDR2( K1, K2, N, LIST1, LIST2 )
                REAL   , INTENT(IN   ) :: K1             !  first  key
                REAL   , INTENT(IN   ) :: K2             !  second key
                INTEGER, INTENT(IN   ) :: N              !  table size
                REAL   , INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                REAL   , INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                END FUNCTION FINDR2

                INTEGER FUNCTION FINDR3( K1, K2, K3,
     &                                   N, LIST1, LIST2, LIST3 )
                REAL   , INTENT(IN   ) :: K1             !  first  key
                REAL   , INTENT(IN   ) :: K2             !  second key
                REAL   , INTENT(IN   ) :: K3             !  third  key
                INTEGER, INTENT(IN   ) :: N              !  table size
                REAL   , INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                REAL   , INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                REAL   , INTENT(IN   ) :: LIST3( N )     !  table to search for K3
                END FUNCTION FINDR3

                INTEGER FUNCTION FINDR4( K1, K2, K3, K4,
     &                                   N, LIST1, LIST2, LIST3, LIST4 )
                REAL   , INTENT(IN   ) :: K1             !  first  key
                REAL   , INTENT(IN   ) :: K2             !  second key
                REAL   , INTENT(IN   ) :: K3             !  third  key
                REAL   , INTENT(IN   ) :: K4             !  third  key
                INTEGER, INTENT(IN   ) :: N              !  table size
                REAL   , INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                REAL   , INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                REAL   , INTENT(IN   ) :: LIST3( N )     !  table to search for K3
                REAL   , INTENT(IN   ) :: LIST4( N )     !  table to search for K4
                END FUNCTION FINDR4

            END INTERFACE       !!  findkey()

            INTERFACE
                INTEGER  FUNCTION GCD( P , Q )
                    INTEGER, INTENT(IN   ) :: P , Q
                END FUNCTION GCD
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION GETDATE ( DEFAULT , PROMPT )
                INTEGER      , INTENT(IN   ) :: DEFAULT         !  Default return date, YYYYDDD
                CHARACTER*(*), INTENT(IN   ) :: PROMPT          !  Prompt for user
                END FUNCTION GETDATE
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION GETDFILE( LNAME, RDONLY,
     &                                     FMTFLAG, RECLEN, CALLER )
                CHARACTER*(*), INTENT(IN   ) :: LNAME          !  logical file name
                LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
                LOGICAL      , INTENT(IN   ) :: FMTFLAG        !  TRUE iff file should be formatted
                INTEGER      , INTENT(IN   ) :: RECLEN         !  record length for direct access
                CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging
                END FUNCTION GETDFILE
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION GETEFILE( LNAME,
     &                                     RDONLY, FMTFLAG, CALLER )
                CHARACTER*(*), INTENT(IN   ) :: LNAME          !  logical file name
                LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
                LOGICAL      , INTENT(IN   ) :: FMTFLAG        !  TRUE iff file should be formatted
                CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging
                END FUNCTION GETEFILE
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION GETFFILE( LNAME, RDONLY, FMTFLAG,
     &                                     RECLEN, CALLER )
                CHARACTER*(*), INTENT(IN   ) :: LNAME          !  logical file name
                LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
                LOGICAL      , INTENT(IN   ) :: FMTFLAG        !  TRUE iff file should be formatted
                INTEGER      , INTENT(IN   ) :: RECLEN         !  record length
                CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging
                END FUNCTION GETFFILE
            END INTERFACE

            INTERFACE
                SUBROUTINE GETSTR ( PROMPT, DEFAULT, RESPONSE )
                CHARACTER*(*), INTENT(IN   ) :: PROMPT, DEFAULT
                CHARACTER*(*), INTENT(  OUT) :: RESPONSE
                END SUBROUTINE GETSTR
            END INTERFACE                      !  GETREAL

            INTERFACE
                SUBROUTINE GETDTTIME( JDATE, JTIME )
                INTEGER, INTENT(  OUT) :: JDATE     !!  date YYYYDDD for "now"
                INTEGER, INTENT(  OUT) :: JTIME     !!  time  HHMMSS for "now"
                END SUBROUTINE GETDTTIME
            END INTERFACE

            INTERFACE GETVAL

                REAL*8 FUNCTION GETDBLE( LO, HI, DEFAULT, PROMPT )
                REAL*8       , INTENT(IN   ) :: LO , HI, DEFAULT
                CHARACTER*(*), INTENT(IN   ) :: PROMPT
                END FUNCTION GETDBLE

                REAL*8 FUNCTION GETDBLE1( DEFAULT, PROMPT )
                REAL*8       , INTENT(IN   ) :: DEFAULT
                CHARACTER*(*), INTENT(IN   ) :: PROMPT
                END FUNCTION GETDBLE1

                INTEGER FUNCTION GETMENU( ITEMCNT, DEFAULT,
     &                                    PROMPT, CHOICES )
                INTEGER      , INTENT(IN   ) :: ITEMCNT         !  number of choices
                INTEGER      , INTENT(IN   ) :: DEFAULT         !  default response
                CHARACTER*(*), INTENT(IN   ) :: PROMPT          !  prompt string
                CHARACTER*(*), INTENT(IN   ) :: CHOICES ( * )   !  array of choice strings
                END FUNCTION GETMENU

                INTEGER FUNCTION GETNUM ( LO , HI , DEFAULT , PROMPT )
                INTEGER      , INTENT(IN   ) :: LO, HI, DEFAULT
                CHARACTER*(*), INTENT(IN   ) :: PROMPT
                END FUNCTION GETNUM

                INTEGER FUNCTION GETNUM1( DEFAULT , PROMPT )
                INTEGER      , INTENT(IN   ) :: DEFAULT
                CHARACTER*(*), INTENT(IN   ) :: PROMPT
                END FUNCTION GETNUM1

                REAL   FUNCTION GETREAL ( LO , HI , DEFAULT , PROMPT )
                REAL         , INTENT(IN   ) :: LO , HI , DEFAULT
                CHARACTER*(*), INTENT(IN   ) :: PROMPT
                END FUNCTION GETREAL

                REAL   FUNCTION GETREAL1( DEFAULT , PROMPT )
                REAL         , INTENT(IN   ) :: DEFAULT
                CHARACTER*(*), INTENT(IN   ) :: PROMPT
                END FUNCTION GETREAL1

                LOGICAL FUNCTION  GETYN ( PROMPT , DEFAULT )
                CHARACTER*(*), INTENT(IN   ) :: PROMPT
                LOGICAL      , INTENT(IN   ) :: DEFAULT
                END FUNCTION  GETYN

            END INTERFACE       !!  getval()

            INTERFACE
                LOGICAL FUNCTION GRDCHK3( FNAME,
     &                                P_ALP, P_BET, P_GAM, XCENT, YCENT,
     &                                XORIG, YORIG, XCELL, YCELL,
     &                                NLAYS, VGTYP, VGTOP, VGLEV )
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
                END FUNCTION GRDCHK3
            END INTERFACE

            INTERFACE
                SUBROUTINE GTPZ0( CRDIN, INSYS, INZONE, TPARIN, INUNIT,
     &                            INSPH, IPR, JPR, LEMSG, LPARM,
     &                            CRDIO, IOSYS, IOZONE, TPARIO, IOUNIT,
     &                            LN27, LN83, FN27, FN83, LENGTH, IFLG )
                REAL*8 , INTENT( IN ) :: CRDIN(2), TPARIN(15)
                INTEGER, INTENT( IN ) :: INSYS, INZONE, INUNIT, INSPH
                INTEGER, INTENT( IN ) :: IPR, JPR, LEMSG, LPARM, IOUNIT
                INTEGER, INTENT( IN ) :: LN27, LN83, LENGTH
                CHARACTER(LEN=128), INTENT( IN ) :: FN27, FN83
                REAL*8 , INTENT( OUT ) :: CRDIO(2), TPARIO(15)
                INTEGER, INTENT( OUT ) :: IFLG
                END SUBROUTINE GTPZ0
            END INTERFACE

            INTERFACE
                CHARACTER(LEN=10) FUNCTION  HHMMSS( JTIME )
                INTEGER, INTENT(IN   ) :: JTIME   !  Julian time, coded YYYYDDD
                END FUNCTION  HHMMSS
            END INTERFACE


            INTERFACE INDEXKEY

                INTEGER FUNCTION INDEX1( NAME, N, NLIST )
                CHARACTER*(*), INTENT(IN   ) :: NAME        !  Character string being searched for
                INTEGER      , INTENT(IN   ) :: N           !  Length of array to be searched
                CHARACTER*(*), INTENT(IN   ) :: NLIST(*)    !  array to be searched
                END FUNCTION INDEX1

                INTEGER FUNCTION INDEXINT1( IKEY, N, NLIST )
                INTEGER, INTENT(IN   ) :: IKEY        !  integer being searched for
                INTEGER, INTENT(IN   ) :: N           !  Length of array to be searched
                INTEGER, INTENT(IN   ) :: NLIST(*)    !  array to be searched
                END FUNCTION INDEXINT1

                INTEGER FUNCTION INDEXL1( LKEY, N, NLIST )
                INTEGER*8, INTENT(IN   ) :: LKEY        !  integer being searched for
                INTEGER,   INTENT(IN   ) :: N           !  Length of array to be searched
                INTEGER*8, INTENT(IN   ) :: NLIST(*)    !  array to be searched
                END FUNCTION INDEXL1

            END INTERFACE       !!  indexkey

            INTERFACE
                LOGICAL FUNCTION ISDSTIME( JDATE )
                INTEGER, INTENT( IN ) :: JDATE   !  Julian date, coded YYYYDDD
                END FUNCTION ISDSTIME
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION JSTEP3( JDATE, JTIME,
     &                                   SDATE, STIME, TSTEP )
                INTEGER, INTENT(IN   ) :: JDATE   !  requested date YYYYDDD
                INTEGER, INTENT(IN   ) :: JTIME   !  requested time HHMMSS
                INTEGER, INTENT(IN   ) :: SDATE   !  starting date  YYYYDDD
                INTEGER, INTENT(IN   ) :: STIME   !  starting time  HHMMSS
                INTEGER, INTENT(IN   ) :: TSTEP   !  time step      H*MMSS
                END FUNCTION JSTEP3
            END INTERFACE

             INTERFACE
                INTEGER FUNCTION JULIAN( YEAR, MNTH, MDAY )
                INTEGER, INTENT(IN   ) :: YEAR            ! year YYYY
                INTEGER, INTENT(IN   ) :: MNTH            ! month 1...12
                INTEGER, INTENT(IN   ) :: MDAY            ! day-of-month 1...28,29,30,31
                END FUNCTION JULIAN
            END INTERFACE

             INTERFACE
                INTEGER FUNCTION JUNIT()
                END FUNCTION JUNIT
            END INTERFACE

            INTERFACE
                SUBROUTINE LASTTIME( SDATE,STIME,TSTEP,NRECS, 
     &                               EDATE,ETIME )
                INTEGER, INTENT(IN   ) :: SDATE, STIME, TSTEP, NRECS
                INTEGER, INTENT(  OUT) :: EDATE, ETIME
                END SUBROUTINE LASTTIME
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION LBLANK( STRING )
                CHARACTER*(*), INTENT( IN ) ::   STRING
                END FUNCTION LBLANK
            END INTERFACE

           INTERFACE LOCATE

                INTEGER FUNCTION LOCAT1( K1, N, LIST1 )
                INTEGER, INTENT(IN   ) :: K1             !  first  key
                INTEGER, INTENT(IN   ) :: N              !  table size
                INTEGER, INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                END FUNCTION LOCAT1

                INTEGER FUNCTION LOCAT2( K1, K2, N, LIST1, LIST2 )
                INTEGER, INTENT(IN   ) :: K1             !  first  key
                INTEGER, INTENT(IN   ) :: K2             !  second key
                INTEGER, INTENT(IN   ) :: N              !  table size
                INTEGER, INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                INTEGER, INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                END FUNCTION LOCAT2

                INTEGER FUNCTION LOCAT3( K1, K2, K3, N,
     &                                   LIST1, LIST2, LIST3, LIST4 )
                INTEGER, INTENT(IN   ) :: K1             !  first  key
                INTEGER, INTENT(IN   ) :: K2             !  second key
                INTEGER, INTENT(IN   ) :: K3             !  third  key
                INTEGER, INTENT(IN   ) :: N              !  table size
                INTEGER, INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                INTEGER, INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                INTEGER, INTENT(IN   ) :: LIST3( N )     !  table to search for K3
                END FUNCTION LOCAT3

                INTEGER FUNCTION LOCAT4( K1, K2, K3, K4, N,
     &                                   LIST1, LIST2, LIST3, LIST4 )
                INTEGER, INTENT(IN   ) :: K1             !  first  key
                INTEGER, INTENT(IN   ) :: K2             !  second key
                INTEGER, INTENT(IN   ) :: K3             !  third  key
                INTEGER, INTENT(IN   ) :: K4             !  fourth key
                INTEGER, INTENT(IN   ) :: N              !  table size
                INTEGER, INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                INTEGER, INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                INTEGER, INTENT(IN   ) :: LIST3( N )     !  table to search for K3
                INTEGER, INTENT(IN   ) :: LIST4( N )     !  table to search for K4
                END FUNCTION LOCAT4

                INTEGER(8) FUNCTION LOCATL1( K1, N, LIST1 )
                INTEGER(8), INTENT(IN   ) :: K1             !  first  key
                INTEGER   , INTENT(IN   ) :: N              !  table size
                INTEGER(8), INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                END FUNCTION LOCATL1

                INTEGER(8) FUNCTION LOCATL2( K1, K2, N, LIST1, LIST2 )
                INTEGER(8), INTENT(IN   ) :: K1             !  first  key
                INTEGER(8), INTENT(IN   ) :: K2             !  second key
                INTEGER   , INTENT(IN   ) :: N              !  table size
                INTEGER(8), INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                INTEGER(8), INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                END FUNCTION LOCATL2

                INTEGER(8) FUNCTION LOCATL3( K1, K2, K3, N,
     &                                   LIST1, LIST2, LIST3, LIST4 )
                INTEGER(8), INTENT(IN   ) :: K1             !  first  key
                INTEGER(8), INTENT(IN   ) :: K2             !  second key
                INTEGER(8), INTENT(IN   ) :: K3             !  third  key
                INTEGER   , INTENT(IN   ) :: N              !  table size
                INTEGER(8), INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                INTEGER(8), INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                INTEGER(8), INTENT(IN   ) :: LIST3( N )     !  table to search for K3
                END FUNCTION LOCATL3

                INTEGER(8) FUNCTION LOCATL4( K1, K2, K3, K4, N,
     &                                   LIST1, LIST2, LIST3, LIST4 )
                INTEGER(8), INTENT(IN   ) :: K1             !  first  key
                INTEGER(8), INTENT(IN   ) :: K2             !  second key
                INTEGER(8), INTENT(IN   ) :: K3             !  third  key
                INTEGER(8), INTENT(IN   ) :: K4             !  fourth key
                INTEGER   , INTENT(IN   ) :: N              !  table size
                INTEGER(8), INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                INTEGER(8), INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                INTEGER(8), INTENT(IN   ) :: LIST3( N )     !  table to search for K3
                INTEGER(8), INTENT(IN   ) :: LIST4( N )     !  table to search for K4
                END FUNCTION LOCATL4

                INTEGER FUNCTION LOCATC( KEY, N, LIST )
                CHARACTER*(*), INTENT(IN   ) :: KEY            !  first  key
                INTEGER      , INTENT(IN   ) :: N              !  table size
                CHARACTER*(*), INTENT(IN   ) :: LIST( N )      !  table to search for KEY
                END FUNCTION LOCATC

                INTEGER FUNCTION LOCATR1( K1, N, LIST1 )
                REAL   , INTENT(IN   ) :: K1             !  first  key
                INTEGER, INTENT(IN   ) :: N              !  table size
                REAL   , INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                END FUNCTION LOCATR1

                INTEGER FUNCTION LOCATR2( K1, K2, N, LIST1, LIST2 )
                REAL   , INTENT(IN   ) :: K1             !  first  key
                REAL   , INTENT(IN   ) :: K2             !  second key
                INTEGER, INTENT(IN   ) :: N              !  table size
                REAL   , INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                REAL   , INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                END FUNCTION LOCATR2

                INTEGER FUNCTION LOCATR3( K1, K2, K3, N,
     &                                    LIST1, LIST2, LIST3 )
                REAL   , INTENT(IN   ) :: K1             !  first  key
                REAL   , INTENT(IN   ) :: K2             !  second key
                REAL   , INTENT(IN   ) :: K3             !  third  key
                INTEGER, INTENT(IN   ) :: N              !  table size
                REAL   , INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                REAL   , INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                REAL   , INTENT(IN   ) :: LIST3( N )     !  table to search for K3
                END FUNCTION LOCATR3

                INTEGER FUNCTION LOCATR4( K1, K2, K3, K4, N,
     &                                    LIST1, LIST2, LIST3, LIST4 )
                REAL   , INTENT(IN   ) :: K1             !  first  key
                REAL   , INTENT(IN   ) :: K2             !  second key
                REAL   , INTENT(IN   ) :: K3             !  third  key
                REAL   , INTENT(IN   ) :: K4             !  fourth key
                INTEGER, INTENT(IN   ) :: N              !  table size
                REAL   , INTENT(IN   ) :: LIST1( N )     !  table to search for K1
                REAL   , INTENT(IN   ) :: LIST2( N )     !  table to search for K2
                REAL   , INTENT(IN   ) :: LIST3( N )     !  table to search for K3
                REAL   , INTENT(IN   ) :: LIST4( N )     !  table to search for K4
                END FUNCTION LOCATR4

            END INTERFACE       !!  locate()

            INTERFACE
                SUBROUTINE  LUSTR( STRING )
                CHARACTER*(*), INTENT(INOUT) :: STRING
                END SUBROUTINE  LUSTR
            END INTERFACE

            INTERFACE
                SUBROUTINE M3EXIT( CALLER, JDATE, JTIME, MSGTXT, ISTAT )
                CHARACTER*(*), INTENT(IN   ) :: CALLER          !  name of the caller
                INTEGER      , INTENT(IN   ) :: JDATE, JTIME    !  model date&time for the error
                CHARACTER*(*), INTENT(IN   ) :: MSGTXT          !  error message
                INTEGER      , INTENT(IN   ) :: ISTAT           !  exit status for program
                END SUBROUTINE M3EXIT
            END INTERFACE

            INTERFACE
                SUBROUTINE  M3FLUSH( IDEV )
                INTEGER, INTENT(IN   ) :: IDEV
                END SUBROUTINE  M3FLUSH
            END INTERFACE

            INTERFACE
                SUBROUTINE  M3MESG( MESSAGE )
                CHARACTER*(*), INTENT(IN   ) :: MESSAGE
                END SUBROUTINE  M3MESG
            END INTERFACE

            INTERFACE
                SUBROUTINE  M3MSG2( MESSAGE )
                CHARACTER*(*), INTENT(IN   ) :: MESSAGE
                END SUBROUTINE  M3MSG2
            END INTERFACE

            INTERFACE
                SUBROUTINE  M3PARAG( NMESG, MSGS )
                INTEGER      , INTENT(IN   ) :: NMESG
                CHARACTER*(*), INTENT(IN   ) :: MSGS( NMESG )
                END SUBROUTINE  M3PARAG
            END INTERFACE

            INTERFACE
                SUBROUTINE  M3PROMPT( MESSAGE, ANSWER, ISTAT )
                CHARACTER*(*), INTENT(IN   ) :: MESSAGE
                CHARACTER*(*), INTENT(  OUT) :: ANSWER
                INTEGER      , INTENT(  OUT) :: ISTAT
                END SUBROUTINE  M3PROMPT
            END INTERFACE

            INTERFACE
                SUBROUTINE  M3WARN( CALLER, JDATE, JTIME, MSGTXT )
                CHARACTER*(*), INTENT(IN   ) :: CALLER          !  name of the caller
                INTEGER      , INTENT(IN   ) :: JDATE, JTIME    !  model date&time for the error
                CHARACTER*(*), INTENT(IN   ) :: MSGTXT          !  error message
                END SUBROUTINE  M3WARN
            END INTERFACE

            INTERFACE
                CHARACTER*14 FUNCTION  MMDDYY ( JDATE )
                INTEGER, INTENT(IN   ) :: JDATE    !  Julian date, coded YYYYDDD
                END FUNCTION MMDDYY
            END INTERFACE

            INTERFACE
                SUBROUTINE NEXTIME( JDATE , JTIME, DTIME )
                INTEGER, INTENT(INOUT) :: JDATE           !  date (encoded YYYYDDD)
                INTEGER, INTENT(INOUT) :: JTIME           !  time (encoded  HHMMSS)
                INTEGER, INTENT(IN   ) :: DTIME           !  time increment (encoded HHMMSS)
                END SUBROUTINE NEXTIME
            END INTERFACE

            INTERFACE
                SUBROUTINE  PCOEF( N, X, Y, C )
                INTEGER, INTENT(IN   ) :: N
                REAL   , INTENT(IN   ) :: X( N )
                REAL   , INTENT(IN   ) :: Y( N )
                REAL   , INTENT(  OUT) :: C( N )
                END SUBROUTINE  PCOEF
            END INTERFACE

            INTERFACE PMATVEC

                SUBROUTINE  PMATVEC11( M, N, P, NX, IX, U, V )
                INTEGER, INTENT(IN   ) :: M             ! length of input vector
                INTEGER, INTENT(IN   ) :: N             ! length of output vector
                INTEGER, INTENT(IN   ) :: P             ! max number of coefficients
                INTEGER, INTENT(IN   ) :: NX( N )       ! # of entries per row
                INTEGER, INTENT(IN   ) :: IX( P )       ! columns list
                REAL   , INTENT(IN   ) ::  U( M )       !  input vector
                REAL   , INTENT(  OUT) ::  V( N )       ! output vector
                END SUBROUTINE  PMATVEC11

                SUBROUTINE  PMATVEC12( M, NC, NR, P, NX, IX, U, V )
                INTEGER, INTENT(IN   ) :: M             ! length of input vector
                INTEGER, INTENT(IN   ) :: NC, NR        ! length of output vector
                INTEGER, INTENT(IN   ) :: P             ! max number of coefficients
                INTEGER, INTENT(IN   ) :: NX( NC*NR )   ! # of entries per row
                INTEGER, INTENT(IN   ) :: IX( P )       ! columns list
                REAL   , INTENT(IN   ) :: U( M )        !  input vector
                REAL   , INTENT(  OUT) :: V( NC,NR )    ! output vector
                END SUBROUTINE  PMATVEC12

                SUBROUTINE  PMATVEC21( MC, MR, N, P, NX, IX, U, V )
                INTEGER, INTENT(IN   ) :: MC, MR        ! length of input vector
                INTEGER, INTENT(IN   ) :: N             ! length of output vector
                INTEGER, INTENT(IN   ) :: P             ! max number of coefficients
                INTEGER, INTENT(IN   ) :: NX( N )       ! # of entries per row
                INTEGER, INTENT(IN   ) :: IX( P )       ! columns list
                REAL   , INTENT(IN   ) :: U( MC,MR )    !  input vector
                REAL   , INTENT(  OUT) :: V( N )        ! output vector
                END SUBROUTINE  PMATVEC21

                SUBROUTINE  PMATVEC22( MC, MR, NC, NR, P, NX, IX, U, V )
                INTEGER, INTENT(IN   ) :: MC, MR        ! length of input vector
                INTEGER, INTENT(IN   ) :: NC, NR        ! length of output vector
                INTEGER, INTENT(IN   ) :: P             ! max number of coefficients
                INTEGER, INTENT(IN   ) :: NX( NC*NR )   ! # of entries per row
                INTEGER, INTENT(IN   ) :: IX( P )       ! columns list
                REAL   , INTENT(IN   ) :: U( MC, MR )   !  input vector
                REAL   , INTENT(  OUT) :: V( NC, NR )   ! output vector
                END SUBROUTINE  PMATVEC22

            END INTERFACE

            INTERFACE
                REAL FUNCTION  POLY( XPT, XPTS, YPTS, NDEG )
                INTEGER, INTENT(IN   ) :: NDEG
                REAL   , INTENT(IN   ) :: XPT
                REAL   , INTENT(IN   ) :: XPTS ( NDEG + 1 )
                REAL   , INTENT(IN   ) :: YPTS ( NDEG + 1 )
                END  FUNCTION  POLY
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION PROMPTDFILE( PROMPT,  RDONLY, FMTTED,
     &                                        RECLEN, DEFAULT, CALLER )
                CHARACTER*(*), INTENT(IN   ) :: PROMPT         !  prompt for user
                LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
                LOGICAL      , INTENT(IN   ) :: FMTTED         !  TRUE iff file should be formatted
                INTEGER	     , INTENT(IN   ) :: RECLEN         !  record length
                CHARACTER*(*), INTENT(IN   ) :: DEFAULT        !  default logical file name
                CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging messages
                END FUNCTION PROMPTDFILE
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION PROMPTFFILE( PROMPT,  RDONLY, FMTTED,
     &                                        DEFAULT, CALLER )
                CHARACTER*(*), INTENT(IN   ) :: PROMPT         !  prompt for user
                LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
                LOGICAL      , INTENT(IN   ) :: FMTTED         !  TRUE iff file should be formatted
                CHARACTER*(*), INTENT(IN   ) :: DEFAULT        !  default logical file name
                CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging messages
                END FUNCTION  PROMPTFFILE
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION PROMPTGRID()
                END FUNCTION PROMPTGRID
            END INTERFACE

            INTERFACE
                CHARACTER*16  FUNCTION PROMPTMFILE( PROMPT,  FMODE,
     &                                              DEFAULT, CALLER )
                CHARACTER*(*), INTENT(IN   ) :: PROMPT         !  prompt for user
                INTEGER      , INTENT(IN   ) :: FMODE          !  file opening-mode
                CHARACTER*(*), INTENT(IN   ) :: DEFAULT        !  default logical file name
                CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging messages
                END FUNCTION  PROMPTMFILE
            END INTERFACE

            INTERFACE
                SUBROUTINE RUNSPEC( FNAME, USEENV, 
     &                              SDATE, STIME, TSTEP, NRECS )
                CHARACTER(LEN=*), INTENT(IN   ) :: FNAME        !!  input file
                LOGICAL,          INTENT(IN   ) :: USEENV       !!  input file
                INTEGER,          INTENT(  OUT) :: SDATE        !!  starting date YYYYDDD
                INTEGER,          INTENT(  OUT) :: STIME        !!  starting time  H*MMSS
                INTEGER,          INTENT(  OUT) :: TSTEP        !!  time step      H*MMSS
                INTEGER,          INTENT(  OUT) :: NRECS        !!  Number of records
                END  SUBROUTINE RUNSPEC
            END INTERFACE

            INTERFACE
                SUBROUTINE SCANINT( STRING, VALUE, NCHARS, NDIGITS )
                CHARACTER*(*), INTENT(IN   ) :: STRING
                INTEGER      , INTENT(  OUT) :: VALUE, NCHARS, NDIGITS
                END SUBROUTINE SCANINT
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETENVVAR( LNAME, VALUE )
                CHARACTER*(*), INTENT( IN ) ::   LNAME
                CHARACTER*(*), INTENT( IN ) ::   VALUE
                END FUNCTION SETENVVAR
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETSPHERE( PARM1, PARM2 )
                REAL*8, INTENT(IN   ) :: PARM1, PARM2
                END FUNCTION SETSPHERE
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION INITSPHERES( )
                END FUNCTION INITSPHERES
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SPHEREDAT( INSPHERE, INPARAM, IOPARAM )
                INTEGER, INTENT(  OUT) :: INSPHERE
                REAL*8 , INTENT(  OUT) :: INPARAM( 15 ), IOPARAM( 15 )
                END FUNCTION SPHEREDAT
            END INTERFACE

            INTERFACE
                SUBROUTINE SKIPL( UNIT, NLINES )
                INTEGER, INTENT(IN   ) :: UNIT
                INTEGER, INTENT(IN   ) :: NLINES
                END  SUBROUTINE SKIPL
            END INTERFACE

            INTERFACE
                SUBROUTINE  SORTIC( N, IND, TBLC )
                INTEGER      , INTENT(IN   ) :: N
                INTEGER      , INTENT(INOUT) :: IND( N )
                CHARACTER*(*), INTENT(IN   ) :: TBLC( * )
                END SUBROUTINE  SORTIC
            END INTERFACE

            INTERFACE SORTI
                SUBROUTINE  SORTIC4( N, IND, TBLC )
                INTEGER      , INTENT(IN   ) :: N
                INTEGER      , INTENT(INOUT) :: IND( N )
                CHARACTER*(*), INTENT(IN   ) :: TBLC( * )
                END SUBROUTINE  SORTIC4

                SUBROUTINE  SORTIC8( N, IND, TBLC )
                INTEGER(8)   , INTENT(IN   ) :: N
                INTEGER(8)   , INTENT(INOUT) :: IND( N )
                CHARACTER*(*), INTENT(IN   ) :: TBLC( * )
                END SUBROUTINE  SORTIC8

                SUBROUTINE  SORTINC4( N, M, IND, TBLC )
                INTEGER      , INTENT(IN   ) :: N
                INTEGER      , INTENT(IN   ) :: M
                INTEGER      , INTENT(INOUT) :: IND( N )
                CHARACTER*(*), INTENT(IN   ) :: TBLC( * )
                END SUBROUTINE  SORTINC4

                SUBROUTINE  SORTINC8( N, M, IND, TBLC )
                INTEGER(8)   , INTENT(IN   ) :: N
                INTEGER(8)   , INTENT(IN   ) :: M
                INTEGER(8)   , INTENT(INOUT) :: IND( N )
                CHARACTER*(*), INTENT(IN   ) :: TBLC( * )
                END SUBROUTINE  SORTINC8

                SUBROUTINE  SORTI1( N, IND, TBL1 )
                INTEGER, INTENT(IN   ) :: N
                INTEGER, INTENT(INOUT) :: IND( N )
                INTEGER, INTENT(IN   ) :: TBL1( * )
                END SUBROUTINE  SORTI1

                SUBROUTINE  SORTI2( N, IND, TBL1, TBL2 )
                INTEGER, INTENT(IN   ) :: N
                INTEGER, INTENT(INOUT) :: IND( N )
                INTEGER, INTENT(IN   ) :: TBL1( * )
                INTEGER, INTENT(IN   ) :: TBL2( * )
                END SUBROUTINE  SORTI2

                SUBROUTINE  SORTI3( N, IND, TBL1, TBL2, TBL3 )
                INTEGER, INTENT(IN   ) :: N
                INTEGER, INTENT(INOUT) :: IND( N )
                INTEGER, INTENT(IN   ) :: TBL1( * )
                INTEGER, INTENT(IN   ) :: TBL2( * )
                INTEGER, INTENT(IN   ) :: TBL3( * )
                END SUBROUTINE  SORTI3

                SUBROUTINE  SORTI4( N, IND, TBL1, TBL2, TBL3, TBL4 )
                INTEGER, INTENT(IN   ) :: N
                INTEGER, INTENT(INOUT) :: IND( N )
                INTEGER, INTENT(IN   ) :: TBL1( * )
                INTEGER, INTENT(IN   ) :: TBL2( * )
                INTEGER, INTENT(IN   ) :: TBL3( * )
                INTEGER, INTENT(IN   ) :: TBL4( * )
                END SUBROUTINE  SORTI4

                SUBROUTINE  SORTL1( N, IND, TBL1 )
                INTEGER,    INTENT(IN   ) :: N
                INTEGER,    INTENT(INOUT) :: IND( N )
                INTEGER(8), INTENT(IN   ) :: TBL1( * )
                END SUBROUTINE  SORTL1

                SUBROUTINE  SORTL2( N, IND, TBL1, TBL2 )
                INTEGER,    INTENT(IN   ) :: N
                INTEGER,    INTENT(INOUT) :: IND( N )
                INTEGER(8), INTENT(IN   ) :: TBL1( * )
                INTEGER(8), INTENT(IN   ) :: TBL2( * )
                END SUBROUTINE  SORTL2

                SUBROUTINE  SORTL3( N, IND, TBL1, TBL2, TBL3 )
                INTEGER,    INTENT(IN   ) :: N
                INTEGER,    INTENT(INOUT) :: IND( N )
                INTEGER(8), INTENT(IN   ) :: TBL1( * )
                INTEGER(8), INTENT(IN   ) :: TBL2( * )
                INTEGER(8), INTENT(IN   ) :: TBL3( * )
                END SUBROUTINE  SORTL3

                SUBROUTINE  SORTL4( N, IND, TBL1, TBL2, TBL3, TBL4 )
                INTEGER,    INTENT(IN   ) :: N
                INTEGER,    INTENT(INOUT) :: IND( N )
                INTEGER(8), INTENT(IN   ) :: TBL1( * )
                INTEGER(8), INTENT(IN   ) :: TBL2( * )
                INTEGER(8), INTENT(IN   ) :: TBL3( * )
                INTEGER(8), INTENT(IN   ) :: TBL4( * )
                END SUBROUTINE  SORTL4

                SUBROUTINE  SORTR1( N, IND, TBL1 )
                INTEGER, INTENT(IN   ) :: N
                INTEGER, INTENT(INOUT) :: IND( N )
                REAL   , INTENT(IN   ) :: TBL1( * )
                END SUBROUTINE  SORTR1

                SUBROUTINE  SORTR2( N, IND, TBL1, TBL2 )
                INTEGER, INTENT(IN   ) :: N
                INTEGER, INTENT(INOUT) :: IND( N )
                REAL   , INTENT(IN   ) :: TBL1( * )
                REAL   , INTENT(IN   ) :: TBL2( * )
                END SUBROUTINE  SORTR2

                SUBROUTINE  SORTR3( N, IND, TBL1, TBL2, TBL3 )
                INTEGER, INTENT(IN   ) :: N
                INTEGER, INTENT(INOUT) :: IND( N )
                REAL   , INTENT(IN   ) :: TBL1( * )
                REAL   , INTENT(IN   ) :: TBL2( * )
                REAL   , INTENT(IN   ) :: TBL3( * )
                END SUBROUTINE  SORTR3

                SUBROUTINE  SORTR4( N, IND, TBL1, TBL2, TBL3, TBL4 )
                INTEGER, INTENT(IN   ) :: N
                INTEGER, INTENT(INOUT) :: IND( N )
                REAL   , INTENT(IN   ) :: TBL1( * )
                REAL   , INTENT(IN   ) :: TBL2( * )
                REAL   , INTENT(IN   ) :: TBL3( * )
                REAL   , INTENT(IN   ) :: TBL4( * )
                END SUBROUTINE  SORTR4

            END INTERFACE       !!  sorti()

            INTERFACE
                SUBROUTINE  SPLITLINE( LINE, NMAX, N, FIELD, EFLAG )
                    CHARACTER(LEN=*), INTENT(  IN )::  LINE
                    INTEGER,          INTENT(  IN )::  NMAX
                    INTEGER,          INTENT( OUT )::  N
                    CHARACTER(LEN=*), INTENT( OUT )::  FIELD( NMAX )
                    LOGICAL,          INTENT( OUT )::  EFLAG    ! error flag
                END SUBROUTINE  SPLITLINE
            END INTERFACE

            INTERFACE
                REAL*8 FUNCTION STR2DBLE( STRING )
                CHARACTER*(*), INTENT(IN   ) :: STRING
                END FUNCTION STR2DBLE
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION STR2INT( STRING )
                CHARACTER*(*), INTENT(IN   ) :: STRING
                END FUNCTION STR2INT
            END INTERFACE

            INTERFACE
                REAL FUNCTION STR2REAL( STRING )
                CHARACTER*(*), INTENT(IN   ) :: STRING
                END FUNCTION STR2REAL
            END INTERFACE

            INTERFACE
                INTEGER  FUNCTION  SEC2TIME ( SECS )
                INTEGER, INTENT(IN   ) :: SECS
                END FUNCTION  SEC2TIME
            END INTERFACE

            INTERFACE
                INTEGER  FUNCTION  TIME2SEC ( TIME )
                INTEGER, INTENT(IN   ) :: TIME    !  formatted HHMMSS
                END FUNCTION  TIME2SEC
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION SECSDIFF( ADATE, ATIME, ZDATE, ZTIME )
                INTEGER, INTENT(IN   ) :: ADATE, ATIME
                INTEGER, INTENT(IN   ) :: ZDATE, ZTIME
                END FUNCTION SECSDIFF
            END INTERFACE

            INTERFACE  UNGRIDB

                SUBROUTINE  UNGRIDBS1( NCOLS1, NROWS1, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NPTS, XLOC, YLOC,
     &                                 NU, CU )
                INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1	!  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
                REAL   , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
                REAL   , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
                INTEGER, INTENT(  OUT) :: NU( 4,NPTS )    !  single-indexed subscripts into grid
                REAL   , INTENT(  OUT) :: CU( 4,NPTS )    !  coefficients
                END SUBROUTINE  UNGRIDBS1

                SUBROUTINE  UNGRIDBS2( NCOLS1, NROWS1, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NCOLS2, NROWS2, XLOC, YLOC,
     &                                 NU, CU )
                INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1        !  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG          !  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL          !  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2        !  dims for grid locations
                REAL   , INTENT(IN   ) :: XLOC( NCOLS2,NROWS2 ) !  X point coordinates
                REAL   , INTENT(IN   ) :: YLOC( NCOLS2,NROWS2 ) !  Y point coordinates
                INTEGER, INTENT(  OUT) :: NU( 4,NCOLS2*NROWS2 ) !  single-indexed subscripts into grid
                REAL   , INTENT(  OUT) :: CU( 4,NCOLS2*NROWS2 ) !  coefficients
                END SUBROUTINE  UNGRIDBS2

                SUBROUTINE  UNGRIDBD1( NCOLS1, NROWS1, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NPTS, XLOC, YLOC,
     &                                 NU, CU )
                INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1	!  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
                REAL*8 , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
                REAL*8 , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
                INTEGER, INTENT(  OUT) :: NU( 4,NPTS )    !  single-indexed subscripts into grid
                REAL   , INTENT(  OUT) :: CU( 4,NPTS )    !  coefficients
                END SUBROUTINE  UNGRIDBD1

                SUBROUTINE  UNGRIDBD2( NCOLS1, NROWS1, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NCOLS2, NROWS2, XLOC, YLOC,
     &                                 NU, CU )
                INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1        !  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG          !  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL          !  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2        !  dims for grid locations
                REAL*8 , INTENT(IN   ) :: XLOC( NCOLS2,NROWS2 ) !  X point coordinates
                REAL*8 , INTENT(IN   ) :: YLOC( NCOLS2,NROWS2 ) !  Y point coordinates
                INTEGER, INTENT(  OUT) :: NU( 4,NCOLS2*NROWS2 ) !  single-indexed subscripts into grid
                REAL   , INTENT(  OUT) :: CU( 4,NCOLS2*NROWS2 ) !  coefficients
                END SUBROUTINE  UNGRIDBD2

                SUBROUTINE  UNGRIDBES1( NCOLS1, NROWS1, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NPTS, XLOC, YLOC,
     &                                 NU, CU, IERR )
                INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1	!  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
                REAL   , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
                REAL   , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
                INTEGER, INTENT(  OUT) :: NU( 4,NPTS )  !  single-indexed subscripts into grid
                REAL   , INTENT(  OUT) :: CU( 4,NPTS )  !  coefficients
                INTEGER, INTENT(  OUT) :: IERR          !  error-count:  0=="no errors"
                END SUBROUTINE  UNGRIDBES1

                SUBROUTINE  UNGRIDBES2( NCOLS1, NROWS1, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NCOLS2, NROWS2, XLOC, YLOC,
     &                                 NU, CU, IERR )
                INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1        !  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG          !  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL          !  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2        !  dims for grid locations
                REAL   , INTENT(IN   ) :: XLOC( NCOLS2,NROWS2 ) !  X point coordinates
                REAL   , INTENT(IN   ) :: YLOC( NCOLS2,NROWS2 ) !  Y point coordinates
                INTEGER, INTENT(  OUT) :: NU( 4,NCOLS2*NROWS2 ) !  single-indexed subscripts into grid
                REAL   , INTENT(  OUT) :: CU( 4,NCOLS2*NROWS2 ) !  coefficients
                INTEGER, INTENT(  OUT) :: IERR                  !  error-count:  0=="no errors"
                END SUBROUTINE  UNGRIDBES2

                SUBROUTINE  UNGRIDBED1( NCOLS1, NROWS1, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NPTS, XLOC, YLOC,
     &                                 NU, CU, IERR )
                INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1	!  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
                REAL*8 , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
                REAL*8 , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
                INTEGER, INTENT(  OUT) :: NU( 4,NPTS )  !  single-indexed subscripts into grid
                REAL   , INTENT(  OUT) :: CU( 4,NPTS )  !  coefficients
                INTEGER, INTENT(  OUT) :: IERR          !  error-count:  0=="no errors"
                END SUBROUTINE  UNGRIDBED1

                SUBROUTINE  UNGRIDBED2( NCOLS1, NROWS1, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NCOLS2, NROWS2, XLOC, YLOC,
     &                                 NU, CU, IERR )
                INTEGER, INTENT(IN   ) :: NCOLS1, NROWS1        !  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG          !  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL          !  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2        !  dims for grid locations
                REAL*8 , INTENT(IN   ) :: XLOC( NCOLS2,NROWS2 ) !  X point coordinates
                REAL*8 , INTENT(IN   ) :: YLOC( NCOLS2,NROWS2 ) !  Y point coordinates
                INTEGER, INTENT(  OUT) :: NU( 4,NCOLS2*NROWS2 ) !  single-indexed subscripts into grid
                REAL   , INTENT(  OUT) :: CU( 4,NCOLS2*NROWS2 ) !  coefficients
                INTEGER, INTENT(  OUT) :: IERR                  !  error-count:  0=="no errors"
                END SUBROUTINE  UNGRIDBED2

            END INTERFACE       !!  ungridb

            INTERFACE UNGRIDI

                SUBROUTINE  UNGRIDIS1( NCOLS, NROWS, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NPTS, XLOC, YLOC,
     &                                 NX )
                INTEGER, INTENT(IN   ) :: NCOLS, NROWS	!  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
                REAL   , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
                REAL   , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
                INTEGER, INTENT(  OUT) :: NX( NPTS )    !  single-indexed subscripts into grid
                END SUBROUTINE  UNGRIDIS1

                SUBROUTINE  UNGRIDIS2( NCOLS, NROWS, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NCOLS2, NROWS2, XLOC, YLOC,
     &                                 NX )
                INTEGER, INTENT(IN   ) :: NCOLS, NROWS	    !  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG	    !  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL	    !  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2    !  number of (point-source) locations
                REAL   , INTENT(IN   ) :: XLOC( NCOLS2,NROWS2 ) !  X point coordinates
                REAL   , INTENT(IN   ) :: YLOC( NCOLS2,NROWS2 ) !  Y point coordinates
                INTEGER, INTENT(  OUT) ::   NX( NCOLS2*NROWS2 ) !  single-indexed subscripts into grid
                END SUBROUTINE  UNGRIDIS2

                SUBROUTINE  UNGRIDID1( NCOLS, NROWS, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NPTS, XLOC, YLOC,
     &                                 NX )
                INTEGER, INTENT(IN   ) :: NCOLS, NROWS	!  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
                REAL*8 , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
                REAL*8 , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
                INTEGER, INTENT(  OUT) :: NX( NPTS )    !  single-indexed subscripts into grid
                END SUBROUTINE  UNGRIDID1

                SUBROUTINE  UNGRIDID2( NCOLS, NROWS, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NCOLS2, NROWS2, XLOC, YLOC,
     &                                 NX )
                INTEGER, INTENT(IN   ) :: NCOLS, NROWS	    !  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG	    !  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL	    !  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2    !  number of (point-source) locations
                REAL*8 , INTENT(IN   ) :: XLOC( NCOLS2,NROWS2 ) !  X point coordinates
                REAL*8 , INTENT(IN   ) :: YLOC( NCOLS2,NROWS2 ) !  Y point coordinates
                INTEGER, INTENT(  OUT) ::   NX( NCOLS2*NROWS2 ) !  single-indexed subscripts into grid
                END SUBROUTINE  UNGRIDID2

                SUBROUTINE  UNGRIDIES1( NCOLS, NROWS, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NPTS, XLOC, YLOC,
     &                                 NX, IERR )
                INTEGER, INTENT(IN   ) :: NCOLS, NROWS	!  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
                REAL   , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
                REAL   , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
                INTEGER, INTENT(  OUT) :: NX( NPTS )    !  single-indexed subscripts into grid
                INTEGER, INTENT(  OUT) :: IERR          !  error-count:  0=="no errors"
                END SUBROUTINE  UNGRIDIES1

                SUBROUTINE  UNGRIDIES2( NCOLS, NROWS, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NCOLS2, NROWS2, XLOC, YLOC,
     &                                 NX, IERR )
                INTEGER, INTENT(IN   ) :: NCOLS, NROWS	    !  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG	    !  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL	    !  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2    !  number of (point-source) locations
                REAL   , INTENT(IN   ) :: XLOC( NCOLS2,NROWS2 ) !  X point coordinates
                REAL   , INTENT(IN   ) :: YLOC( NCOLS2,NROWS2 ) !  Y point coordinates
                INTEGER, INTENT(  OUT) ::   NX( NCOLS2*NROWS2 ) !  single-indexed subscripts into grid
                INTEGER, INTENT(  OUT) :: IERR                  !  error-count:  0=="no errors"
                END SUBROUTINE  UNGRIDIES2

                SUBROUTINE  UNGRIDIED1( NCOLS, NROWS, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NPTS, XLOC, YLOC,
     &                                 NX, IERR )
                INTEGER, INTENT(IN   ) :: NCOLS, NROWS	!  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG	!  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL	!  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NPTS	        !  number of (point-source) locations
                REAL*8 , INTENT(IN   ) :: XLOC( NPTS ) 	!  X point coordinates
                REAL*8 , INTENT(IN   ) :: YLOC( NPTS ) 	!  Y point coordinates
                INTEGER, INTENT(  OUT) :: NX( NPTS )    !  single-indexed subscripts into grid
                INTEGER, INTENT(  OUT) :: IERR          !  error-count:  0=="no errors"
                END SUBROUTINE  UNGRIDIED1

                SUBROUTINE  UNGRIDIED2( NCOLS, NROWS, XORIG, YORIG,
     &                                 XCELL, YCELL,
     &                                 NCOLS2, NROWS2, XLOC, YLOC,
     &                                 NX, IERR )
                INTEGER, INTENT(IN   ) :: NCOLS, NROWS	    !  number of grid columns, rows
                REAL*8 , INTENT(IN   ) :: XORIG, YORIG	    !  X,Y coords of LL grid corner
                REAL*8 , INTENT(IN   ) :: XCELL, YCELL	    !  X,Y direction cell size
                INTEGER, INTENT(IN   ) :: NCOLS2, NROWS2    !  number of (point-source) locations
                REAL*8 , INTENT(IN   ) :: XLOC( NCOLS2,NROWS2 ) !  X point coordinates
                REAL*8 , INTENT(IN   ) :: YLOC( NCOLS2,NROWS2 ) !  Y point coordinates
                INTEGER, INTENT(  OUT) ::   NX( NCOLS2*NROWS2 ) !  single-indexed subscripts into grid
                INTEGER, INTENT(  OUT) :: IERR                  !  error-count:  0=="no errors"
                END SUBROUTINE  UNGRIDIED2

            END INTERFACE       !!  ungridi

            INTERFACE
                SUBROUTINE  UPCASE( BUFFER )
                CHARACTER*(*), INTENT(INOUT) :: BUFFER
                END SUBROUTINE  UPCASE
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION WKDAY( JDATE )
                INTEGER, INTENT(IN   ) :: JDATE	!  date YYYYDDD = 1000 * YEAR + DAY
                END FUNCTION WKDAY
            END INTERFACE

            INTERFACE
                INTEGER FUNCTION YEAR4 ( YY )
                INTEGER, INTENT(IN   ) :: YY    ! 2 digit year
                END FUNCTION YEAR4
            END INTERFACE

            INTERFACE
                REAL FUNCTION YR2DAY ( YEAR )
                INTEGER, INTENT(IN   ) :: YEAR  ! 4 digit year YYYY
                END FUNCTION YR2DAY
            END INTERFACE


!-=-=-=-=-=-=-=-=-=-=-=-=-  LAMBERT()  Related Interfaces  -=-=-=-=-=-=-=-

            INTERFACE
                LOGICAL FUNCTION LAMBERT( CNAME, A, B, C, X, Y )
                CHARACTER(LEN=*), INTENT(IN   ) :: CNAME
                REAL, INTENT(IN   ) :: A          !  first secant latitude
                REAL, INTENT(IN   ) :: B          !  second secant latitude.  B > A
                REAL, INTENT(IN   ) :: C          !  central meridian
                REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
                END FUNCTION LAMBERT
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION POLSTE( CNAME, A, B, C, X, Y )
                CHARACTER(LEN=*), INTENT(IN   ) :: CNAME
                REAL, INTENT(IN   ) :: A          !  first secant latitude
                REAL, INTENT(IN   ) :: B          !  second secant latitude.  B > A
                REAL, INTENT(IN   ) :: C          !  central meridian
                REAL, INTENT(IN   ) :: X          !  POLSTE easting  in meters
                REAL, INTENT(IN   ) :: Y          !  POLSTE northing in meters
                END FUNCTION POLSTE
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION EQMERC( CNAME, A, B, C, X, Y )
                CHARACTER(LEN=*), INTENT(IN   ) :: CNAME
                REAL, INTENT(IN   ) :: A          !  first secant latitude
                REAL, INTENT(IN   ) :: B          !  second secant latitude.  B > A
                REAL, INTENT(IN   ) :: C          !  central meridian
                REAL, INTENT(IN   ) :: X          !  EQMERC easting  in meters
                REAL, INTENT(IN   ) :: Y          !  EQMERC northing in meters
                END FUNCTION EQMERC
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION TRMERC( CNAME, A, B, C, X, Y )
                CHARACTER(LEN=*), INTENT(IN   ) :: CNAME
                REAL, INTENT(IN   ) :: A          !  first secant latitude
                REAL, INTENT(IN   ) :: B          !  second secant latitude.  B > A
                REAL, INTENT(IN   ) :: C          !  central meridian
                REAL, INTENT(IN   ) :: X          !  TRMERC easting  in meters
                REAL, INTENT(IN   ) :: Y          !  TRMERC northing in meters
                END FUNCTION TRMERC
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION ALBERS( CNAME, A, B, C, X, Y )
                CHARACTER(LEN=*), INTENT(IN   ) :: CNAME
                REAL, INTENT(IN   ) :: A          !  first secant latitude
                REAL, INTENT(IN   ) :: B          !  second secant latitude.  B > A
                REAL, INTENT(IN   ) :: C          !  central meridian
                REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
                END FUNCTION ALBERS
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETLAM( A, B, C, X, Y )
                REAL, INTENT(IN   ) :: A          !  first secant latitude
                REAL, INTENT(IN   ) :: B          !  second secant latitude.  B > A
                REAL, INTENT(IN   ) :: C          !  central meridian
                REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
                END FUNCTION SETLAM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETPOL( A, B, C, X, Y )
                REAL, INTENT(IN   ) :: A          !  first secant latitude
                REAL, INTENT(IN   ) :: B          !  second secant latitude.  B > A
                REAL, INTENT(IN   ) :: C          !  central meridian
                REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
                END FUNCTION SETPOL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETEQM( A, B, C, X, Y )
                REAL, INTENT(IN   ) :: A          !  first secant latitude
                REAL, INTENT(IN   ) :: B          !  second secant latitude.  B > A
                REAL, INTENT(IN   ) :: C          !  central meridian
                REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
                END FUNCTION SETEQM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETTRM( A, B, C, X, Y )
                REAL, INTENT(IN   ) :: A          !  first secant latitude
                REAL, INTENT(IN   ) :: B          !  second secant latitude.  B > A
                REAL, INTENT(IN   ) :: C          !  central meridian
                REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
                END FUNCTION SETTRM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION SETALB( A, B, C, X, Y )
                REAL, INTENT(IN   ) :: A          !  first secant latitude
                REAL, INTENT(IN   ) :: B          !  second secant latitude.  B > A
                REAL, INTENT(IN   ) :: C          !  central meridian
                REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
                END FUNCTION SETALB
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LAM2LL( X, Y, LON, LAT )
                REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
                REAL           LON        !  Lambert easting  in meters
                REAL           LAT        !  Lambert northing in meters
                END FUNCTION LAM2LL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LL2LAM( LON, LAT, X, Y )
                REAL           X          !  Lambert easting  in meters
                REAL           Y          !  Lambert northing in meters
                REAL, INTENT(IN   ) :: LON        !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: LAT        !  Lambert northing in meters
                END FUNCTION LL2LAM
            END INTERFACE

            INTERFACE
                SUBROUTINE LL2UTM( LON, LAT, Z, X, Y )
                REAL   , INTENT(IN   ) :: LON    !  Longitude in decimal degrees
                REAL   , INTENT(IN   ) :: LAT    !  Latitude in decimal degrees
                REAL   , INTENT(  OUT) :: X      !  Returned UTM Easting in meters
                REAL   , INTENT(  OUT) :: Y      !  Returned UTM Northing in meters
                INTEGER, INTENT(IN   ) :: Z      !  UTM zone
                END SUBROUTINE LL2UTM
            END INTERFACE

            INTERFACE
                SUBROUTINE UTM2LL( X, Y, Z, LON, LAT )
                REAL   , INTENT(IN   ) :: X       !  UTM easting  in meters
                REAL   , INTENT(IN   ) :: Y       !  UTM northing in meters
                INTEGER, INTENT(IN   ) :: Z       !  UTM zone
                REAL   , INTENT(  OUT) :: LON     !  East longitude in decimal degrees
                REAL   , INTENT(  OUT) :: LAT     !  North latitude in decimal degrees
                END SUBROUTINE UTM2LL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LAM2UTM( X, Y, Z, U, V )
                REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
                INTEGER, INTENT(IN   ) :: Z          !  UTM zone (1...36)
                REAL, INTENT(  OUT) :: U          !  UTM easting  in meters
                REAL, INTENT(  OUT) :: V          !  UTM northing in meters
                END FUNCTION LAM2UTM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION UTM2LAM( U, V, Z, X, Y )
                REAL, INTENT(IN   ) :: U          !  UTM easting  in meters
                REAL, INTENT(IN   ) :: V          !  UTM northing in meters
                INTEGER, INTENT(IN) :: Z          !  UTM zone (1...36)
                REAL, INTENT(  OUT) :: X          !  Lambert easting  in meters
                REAL, INTENT(  OUT) :: Y          !  Lambert northing in meters
                END FUNCTION UTM2LAM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LAM2POL( X, Y, U, V )
                REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
                REAL, INTENT(  OUT) :: U          !  POL easting  in meters
                REAL, INTENT(  OUT) :: V          !  POL northing in meters
                END FUNCTION LAM2POL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION POL2LAM( U, V, X, Y )
                REAL, INTENT(IN   ) :: U          !  POL easting  in meters
                REAL, INTENT(IN   ) :: V          !  POL northing in meters
                REAL, INTENT(  OUT) :: X          !  Lambert easting  in meters
                REAL, INTENT(  OUT) :: Y          !  Lambert northing in meters
                END FUNCTION POL2LAM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION POL2LL( X, Y, LON, LAT )
                REAL, INTENT(IN   ) :: X          !  POL easting  in meters
                REAL, INTENT(IN   ) :: Y          !  POL northing in meters
                REAL, INTENT(  OUT) :: LON        !  longitude (degrees)
                REAL, INTENT(  OUT) :: LAT        !  latitude  (degrees)
                END FUNCTION POL2LL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LL2POL( LON, LAT, X, Y )
                REAL, INTENT(IN   ) :: LON        !  longitude (degrees)
                REAL, INTENT(IN   ) :: LAT        !  latitude  (degrees)
                REAL, INTENT(  OUT) :: X          !  POL easting  in meters
                REAL, INTENT(  OUT) :: Y          !  POL northing in meters
                END FUNCTION LL2POL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION POL2UTM( X, Y, Z, U, V )
                REAL, INTENT(IN   ) :: X          !  POL easting  in meters
                REAL, INTENT(IN   ) :: Y          !  POL northing in meters
                INTEGER, INTENT(IN) :: Z          !  UTM zone (1...36)
                REAL, INTENT(  OUT) :: U          !  UTM easting  in meters
                REAL, INTENT(  OUT) :: V          !  UTM northing in meters
                END FUNCTION POL2UTM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION UTM2POL( U, V, Z, X, Y )
                REAL, INTENT(IN   ) :: U          !  UTM easting  in meters
                REAL, INTENT(IN   ) :: V          !  UTM northing in meters
                INTEGER, INTENT(IN) :: Z          !  UTM zone (1...36)
                REAL, INTENT(  OUT) :: X          !  POL easting  in meters
                REAL, INTENT(  OUT) :: Y          !  POL northing in meters
                END FUNCTION UTM2POL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION TRM2LL( X, Y, LON, LAT )
                REAL, INTENT(IN   ) :: X          !  TRM easting  in meters
                REAL, INTENT(IN   ) :: Y          !  TRM northing in meters
                REAL, INTENT(  OUT) :: LON        !  longitude (degrees)
                REAL, INTENT(  OUT) :: LAT        !  latitude  (degrees)
                END FUNCTION TRM2LL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LL2TRM( LON, LAT, X, Y )
                REAL, INTENT(IN   ) :: LON        !  longitude (degrees)
                REAL, INTENT(IN   ) :: LAT        !  latitude  (degrees)
                REAL, INTENT(  OUT) :: X          !  TRM easting  in meters
                REAL, INTENT(  OUT) :: Y          !  TRM northing in meters
                END FUNCTION LL2TRM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LAM2TRM( X, Y, U, V )
                REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
                REAL, INTENT(  OUT) :: U          !  TRM easting  in meters
                REAL, INTENT(  OUT) :: V          !  TRM northing in meters
                END FUNCTION LAM2TRM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION TRM2LAM( U, V, X, Y )
                REAL, INTENT(IN   ) :: U          !  TRM easting  in meters
                REAL, INTENT(IN   ) :: V          !  TRM northing in meters
                REAL, INTENT(  OUT) :: X          !  Lambert easting  in meters
                REAL, INTENT(  OUT) :: Y          !  Lambert northing in meters
                END FUNCTION TRM2LAM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION TRM2UTM( X, Y, Z, U, V )
                REAL, INTENT(IN   ) :: X          !  TRM easting  in meters
                REAL, INTENT(IN   ) :: Y          !  TRM northing in meters
                INTEGER, INTENT(IN) :: Z          !  UTM zone (1...36)
                REAL, INTENT(  OUT) :: U          !  UTM easting  in meters
                REAL, INTENT(  OUT) :: V          !  UTM northing in meters
                END FUNCTION TRM2UTM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION UTM2TRM( U, V, Z, X, Y )
                REAL, INTENT(IN   ) :: U          !  UTM easting  in meters
                REAL, INTENT(IN   ) :: V          !  UTM northing in meters
                INTEGER, INTENT(IN) :: Z          !  UTM zone (1...36)
                REAL, INTENT(  OUT) :: X          !  TRM easting  in meters
                REAL, INTENT(  OUT) :: Y          !  TRM northing in meters
                END FUNCTION UTM2TRM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION TRM2POL( U, V, X, Y )
                REAL, INTENT(IN   ) :: U          !  TRM easting  in meters
                REAL, INTENT(IN   ) :: V          !  TRM northing in meters
                REAL, INTENT(  OUT) :: X          !  POL easting  in meters
                REAL, INTENT(  OUT) :: Y          !  POL northing in meters
                END FUNCTION TRM2POL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION POL2TRM( X, Y, U, V )
                REAL, INTENT(IN   ) :: X          !  POL easting  in meters
                REAL, INTENT(IN   ) :: Y          !  POL northing in meters
                REAL, INTENT(  OUT) :: U          !  TRM easting  in meters
                REAL, INTENT(  OUT) :: V          !  TRM northing in meters
                END FUNCTION POL2TRM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION EQM2LL( X, Y, LON, LAT )
                REAL, INTENT(IN   ) :: X          !  EQM easting  in meters
                REAL, INTENT(IN   ) :: Y          !  EQM northing in meters
                REAL, INTENT(  OUT) :: LON        !  longitude (degrees)
                REAL, INTENT(  OUT) :: LAT        !  latitude  (degrees)
                END FUNCTION EQM2LL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LL2EQM( LON, LAT, X, Y )
                REAL, INTENT(IN   ) :: LON        !  longitude (degrees)
                REAL, INTENT(IN   ) :: LAT        !  latitude  (degrees)
                REAL, INTENT(  OUT) :: X          !  EQM easting  in meters
                REAL, INTENT(  OUT) :: Y          !  EQM northing in meters
                END FUNCTION LL2EQM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LAM2EQM( X, Y, U, V )
                REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
                REAL, INTENT(  OUT) :: U          !  EQM easting  in meters
                REAL, INTENT(  OUT) :: V          !  EQM northing in meters
                END FUNCTION LAM2EQM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION EQM2LAM( U, V, X, Y )
                REAL, INTENT(IN   ) :: U          !  EQM easting  in meters
                REAL, INTENT(IN   ) :: V          !  EQM northing in meters
                REAL, INTENT(  OUT) :: X          !  Lambert easting  in meters
                REAL, INTENT(  OUT) :: Y          !  Lambert northing in meters
                END FUNCTION EQM2LAM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION EQM2UTM( X, Y, Z, U, V )
                REAL, INTENT(IN   ) :: X          !  EQM easting  in meters
                REAL, INTENT(IN   ) :: Y          !  EQM northing in meters
                INTEGER, INTENT(IN) :: Z          !  UTM zone (1...36)
                REAL, INTENT(  OUT) :: U          !  UTM easting  in meters
                REAL, INTENT(  OUT) :: V          !  UTM northing in meters
                END FUNCTION EQM2UTM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION UTM2EQM( U, V, Z, X, Y )
                REAL, INTENT(IN   ) :: U          !  UTM easting  in meters
                REAL, INTENT(IN   ) :: V          !  UTM northing in meters
                INTEGER, INTENT(IN) :: Z          !  UTM zone (1...36)
                REAL, INTENT(  OUT) :: X          !  EQM easting  in meters
                REAL, INTENT(  OUT) :: Y          !  EQM northing in meters
                END FUNCTION UTM2EQM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION EQM2POL( U, V, X, Y )
                REAL, INTENT(IN   ) :: U          !  EQM easting  in meters
                REAL, INTENT(IN   ) :: V          !  EQM northing in meters
                REAL, INTENT(  OUT) :: X          !  POL easting  in meters
                REAL, INTENT(  OUT) :: Y          !  POL northing in meters
                END FUNCTION EQM2POL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION POL2EQM( X, Y, U, V )
                REAL, INTENT(IN   ) :: X          !  POL easting  in meters
                REAL, INTENT(IN   ) :: Y          !  POL northing in meters
                REAL, INTENT(  OUT) :: U          !  EQM easting  in meters
                REAL, INTENT(  OUT) :: V          !  EQM northing in meters
                END FUNCTION POL2EQM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION EQM2TRM( U, V, X, Y )
                REAL, INTENT(IN   ) :: U          !  EQM easting  in meters
                REAL, INTENT(IN   ) :: V          !  EQM northing in meters
                REAL, INTENT(  OUT) :: X          !  TRM easting  in meters
                REAL, INTENT(  OUT) :: Y          !  TRM northing in meters
                END FUNCTION EQM2TRM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION TRM2EQM( X, Y, U, V )
                REAL, INTENT(IN   ) :: X          !  TRM easting  in meters
                REAL, INTENT(IN   ) :: Y          !  TRM northing in meters
                REAL, INTENT(  OUT) :: U          !  EQM easting  in meters
                REAL, INTENT(  OUT) :: V          !  EQM northing in meters
                END FUNCTION TRM2EQM
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION ALB2LL( X, Y, LON, LAT )
                REAL, INTENT(IN   ) :: X          !  Albers easting  in meters
                REAL, INTENT(IN   ) :: Y          !  Albers northing in meters
                REAL, INTENT(  OUT) :: LON        !  Lambert easting  in meters
                REAL, INTENT(  OUT) :: LAT        !  Lambert northing in meters
                END FUNCTION ALB2LL
            END INTERFACE

            INTERFACE
                LOGICAL FUNCTION LL2ALB( LON, LAT, X, Y )
                REAL, INTENT(IN   ) :: LON        !  longitude (degrees)
                REAL, INTENT(IN   ) :: LAT        !  latitude  (degrees)
                REAL, INTENT(  OUT) :: X          !  Albers easting  in meters
                REAL, INTENT(  OUT) :: Y          !  Albers northing in meters
                END FUNCTION LL2ALB
            END INTERFACE


!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


        !!........  PRIVATE PARAMETER:

            CHARACTER(LEN=256), PRIVATE, PARAMETER :: ZEROS =
     &'0000000000000000000000000000000000000000000000000000000000000000'
     &//
     &'0000000000000000000000000000000000000000000000000000000000000000'
     &//
     &'0000000000000000000000000000000000000000000000000000000000000000'
     &//
     &'0000000000000000000000000000000000000000000000000000000000000000'


        CONTAINS


            SUBROUTINE FIXFIELD( FIELD )

            ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
            !   DESCRIPTION
            !       Convert "missing" = "-9" fields and leading blanks
            !       in FIELD to all-zeros
            ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            !!  Argument:

            CHARACTER(LEN=*), INTENT( INOUT )::  FIELD

            !!  Local Variables:

            INTEGER         I, L

            !!  begin body..........................................

            L = LEN( FIELD )

            IF( INDEX( FIELD, '-9' ) .GT.  0  .OR.
     &                 FIELD         .EQ. '0'      ) THEN

                FIELD = ZEROS

            ELSE

                DO  I = 1, L
                    IF( FIELD( I:I ) .EQ. ' ' ) THEN
                        FIELD( I:I ) = '0'
                    ELSE
                        EXIT
                    END IF
                END DO

            END IF

            RETURN
            END SUBROUTINE FIXFIELD



        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


            REAL FUNCTION KEYVAL( KEY )

            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
            !  retrieve value of REAL KEY from FDESC3's FDESC3D fields
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            !!........  Arguments:

            CHARACTER(LEN=*), INTENT( IN ) :: KEY


            !!........  Local Variables:

            INTEGER         L, C, K
            REAL            X


            !!........  begin body ........................................

            K = LEN_TRIM( KEY )

            DO L = 1, MXDESC3
                C = INDEX( FDESC3D( L ), KEY )
                IF ( C .GT. 0 ) THEN
                    KEYVAL = STR2REAL( FDESC3D( L )( C+K:80 ) )
                    RETURN
                END IF
            END DO

            KEYVAL = BADVAL3
            RETURN


            END FUNCTION KEYVAL


        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


            SUBROUTINE KEYSTR( KEY, VAL )

            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
            !  retrieve value of KEY from FDESC3's FDESC3D fields
            !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

            !!........  Arguments:

            CHARACTER(LEN=*), INTENT( IN  ) :: KEY
            CHARACTER(LEN=*), INTENT( OUT ) :: VAL


            !!........  Local Variables:

            INTEGER         L, C, K
            REAL            X


            !!........  begin body ........................................

            K = LEN_TRIM( KEY )

            DO L = 1, MXDESC3
                C = INDEX( FDESC3D( L ), KEY )
                IF ( C .GT. 0 ) THEN
                    VAL = TRIM( ADJUSTL( FDESC3D( L )( C+K:80 ) ) )
                    RETURN
                END IF
            END DO

            VAL = CMISS3
            RETURN


            END SUBROUTINE KEYSTR


        ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


            INTEGER FUNCTION LCM( I, J )
                INTEGER, INTENT( IN ) :: I, J
                LCM = ( I * J ) / GCD( I, J )
            END FUNCTION LCM


        END MODULE M3UTILIO

