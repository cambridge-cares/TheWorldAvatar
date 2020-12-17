
      SUBROUTINE UTM2LL( X, Y, Z, LON, LAT )

C***********************************************************************
C Version "$Id: utm2ll.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems, and
C (C) 2013-2016 UNC Institute for the Environment
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  99
C
C  FUNCTION:
C     Convert UTM zone-Z coordinates X-Y to LAT-LON coords
C
C  PRECONDITIONS REQUIRED:  none
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       USGS National Mapping Division General Cartographic Transformation
C       Package, routine GTPZ0()
C
C  REVISION  HISTORY:
C       Prototype 6/1995 by CJC, adapted from UAM EPS MAPUTG() 
C       X,Y are now in meters.
C       Version 10/1995 uses GTPZ0()
C       Version 4/2003 by Carlie J. Coats, Jr., BAMS:  support for 
C       additional (non-GRS80) spheres, via INITSPHERES/SPHEREDAT
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C       Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO
C       Modified 03/2016 by CJC:  rm M3UTILIO:  "gfortran" claims
C       circular dependency ;-(
C***********************************************************************

        IMPLICIT NONE


C...........   ARGUMENTS:

      REAL   , INTENT(IN   ) :: X       !  UTM easting  in meters
      REAL   , INTENT(IN   ) :: Y       !  UTM northing in meters
      INTEGER, INTENT(IN   ) :: Z       !  UTM zone
      REAL   , INTENT(  OUT) :: LON     !  East longitude in decimal degrees
      REAL   , INTENT(  OUT) :: LAT     !  North latitude in decimal degrees
        
C...........   External Functions

        INTEGER, EXTERNAL :: INIT3
        LOGICAL, EXTERNAL :: INITSPHERES, SPHEREDAT


C.......   LOCAL VARIABLES:
C.......   Arguments for GTPZ0:

      REAL*8            CRDIN( 2 )      !  input coordinates x,y
      INTEGER           INSYS           !  input projection code
      INTEGER           INZONE          !  input utm zone, etc.
      REAL*8            TPARIN( 15 )    !  input projection parameters
      INTEGER           INUNIT          !  input units code
      INTEGER           INSPH           !  spheroid code
      INTEGER           IPR             !  error print flag
      INTEGER           JPR             !  projection parameter print flag
      INTEGER           LEMSG           !  error message unit number
      INTEGER           LPARM           !  projection parameter unit number
      REAL*8            CRDIO( 2 )      !  output coordinates x,y
      INTEGER           IOSYS           !  output projection code
      INTEGER           IOZONE          !  output utm zone, etc.
      REAL*8            TPARIO( 15 )    !  output projection parameters
      INTEGER           IOUNIT          !  output units code
      INTEGER           LN27            !  NAD1927 file unit number
      INTEGER           LN83            !  NAD1983 file unit number
      CHARACTER*128     FN27            !  NAD1927 file name
      CHARACTER*128     FN83            !  NAD1983 file name
      INTEGER           LENGTH          !  NAD* record-length
      INTEGER           IFLG            !  error flag

C...........   SAVED LOCAL VARIABLES and their descriptions:
C...........   NOTE:  the ANSI standard requires the use of SAVE statements
C...........   for variables which must retain their values from call to call.

        LOGICAL, SAVE :: FIRSTIME = .TRUE.

C.......   Error codes for GTPZ0:

      CHARACTER*64, PARAMETER :: MESG( 9 ) = (/
     &  'Illegal input system code INSYS                ',
     &  'Illegal output system code IOSYS               ',
     &  'Illegal input unit code INUNIT                 ',
     &  'Illegal output unit code IOUNIT                ',
     &  'Inconsistent unit and system codes for input   ',
     &  'Inconsistent unit and system codes for output  ',
     &  'Illegal input zone code INZONE                 ',
     &  'Illegal output zone code IOZONE                ',
     &  'Projection-specific error                      ' /)


C***********************************************************************
C   begin body of subroutine  UTM2LL()

        IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            IF ( .NOT. INITSPHERES() ) THEN
                CALL M3WARN( 'UTM2LL',0,0,'Bad geodetic sphere info' )
            END IF
        END IF          !  if firstime

C.......   Set up input arguments for GTPZ0()

        CRDIN( 1 ) = DBLE( X )
        CRDIN( 2 ) = DBLE( Y )
        INSYS  = 1
        INZONE = Z
        INUNIT = 2       !  meters
        INSPH  = 8       !  GRS 1980 spheroid
        IPR    = 0       !  print error messages, if any
        JPR    = 1       !  do NOT print projection parameters
        LEMSG  = INIT3() !  unit number for log file
        LPARM  = LEMSG   !  projection parameters file
        IOSYS  = 0       !  geographic (lat-lon)
        IOUNIT = 4       !  output units: degrees
        
C.......   Set up and call GTPZ0()

        IF ( .NOT.SPHEREDAT( INSPH, TPARIN, TPARIO ) ) THEN
            CALL M3WARN( 'UTM2LL',0,0,'Bad geodetic sphere info' )
        END IF

        CALL GTPZ0( CRDIN, INSYS, INZONE, TPARIN, INUNIT, INSPH, 
     &              IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE, 
     &              TPARIO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH, 
     &              IFLG )

        IF ( IFLG .NE. 0 ) THEN
            IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
            CALL M3WARN( 'UTM2LL', 0,0, MESG( IFLG ) )
        END IF

C.......   Decode output arguments for GTPZ0()

      LON = SNGL( CRDIO( 1 ) )
      LAT = SNGL( CRDIO( 2 ) )


        RETURN
        END SUBROUTINE UTM2LL

