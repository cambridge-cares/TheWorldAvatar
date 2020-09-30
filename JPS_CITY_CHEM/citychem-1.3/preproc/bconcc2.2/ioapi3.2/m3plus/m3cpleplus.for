
        PROGRAM M3CPLE

    !!***********************************************************************
    !! Version "$Id: m3cple.f90 108 2018-09-07 18:59:37Z coats $"
    !!   EDSS/Models-3 M3TOOLS.
    !!   Copyright (C) 1992-2002 MCNC,
    !!   (C) 1995-2002,2005-2013, 2018- Carlie J. Coats, Jr.,
    !!   (C) 2003-2010 Baron Advanced Meteorological Systems. LLC., and
    !!   (C) 2014-2016 UNC Institute for the Environment.
    !!   Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !!   See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  136
    !!
    !!  DESCRIPTION:
    !!       For each time step in the specified time step sequence,
    !!       reads all variables from the specified input file, optionally
    !!       under the control of the specified synchronization file,
    !!       interpolate it to the specified output grid, and write them
    !!       to the specified output file.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       setenv <logical name> <physical (path) name> for the input,
    !!       output, and GRIDDESC files.
    !!       Input file and output grid use the same coordinate system.
    !!       Specified time step sequence is valid for both the input and
    !!       synch files.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       I/O API
    !!
    !!  REVISION  HISTORY:
    !!       Prototype 8/99 by Carlie J. Coats, Jr., NCSC
    !!       Version  11/2001 by CJC for I/O API Version 2.1
    !!       Version  10/2002 by CJC for I/O API Version 2.2:  support for
    !!       many additional coordinate transformations, direct calls to GCTP
    !!       Version   6/2005 by CJC:  improved default for NRECS
    !!       Version  11/2005 by CJC:  eliminate unused vbles and functions
    !!       Version   9/2006 by CJC:  sphere/spheroid support
    !!       Version   3/2008 by CJC:  INSPHERE default-value bug-fix.
    !!       No resolution-warning for to/from-Lat-Lon conversions.
    !!       Version    6/2008 by CJC:  Albers map-projection support,
    !!       additional spheres 20, 21 from Steve Howard, NOAA/ASMD
    !!       Version   12/2008 by CJC:  heuristic to compensate for WMO screw-up
    !!       that declares all longitudes should be positive
    !!       Version   4/2009 by CJC:  corrections for TRM, EQM
    !!       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!       USE M3UTILIO, and related changes.
    !!       Version  01/2013 by CJC:  use new LASTTIME() to find EDATE:ETIME
    !!       Version  12/2014 by CJC for I/O API v3.2:  USE MODGCTP: GRID2INDX(),
    !!       INDXMULT();  F90 free-format source; use generics for "GET*()"
    !!       Version  06/2016 by CJC:  copy CMAQ metadata, if present
    !!       Version  01/2018 by CJC:  copy-operations for variables of
    !!       types M3INT, M3DBLE.
    !!       Version  08/2018 by CJC:  fix coordinate-check error
    !!***********************************************************************

      USE M3UTILIO
      USE MODGCTP
      USE MODATTS3

      IMPLICIT NONE

      !!...........   PARAMETERS and their descriptions:

      CHARACTER*16, PARAMETER :: PNAME = 'M3CPLE'
      CHARACTER*16, PARAMETER :: BLANK = ' '
      CHARACTER*72, PARAMETER :: BAR = '-=-=-=-=-=-=-=-=-=-=-=-=-=-'
 !     &  BAR = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


      !!...........   LOCAL VARIABLES and their descriptions:

      CHARACTER*16    FNAME   !  input data  file logical name
      CHARACTER*16    SNAME   !  input synch file logical name
      CHARACTER*16    SVBLE   !  input   synch variable   name
      CHARACTER*16    ONAME   !  output data file logical name
      CHARACTER*16    CNAME   !  output coordinate system name
      CHARACTER*16    GNAME   !  output grid name

      LOGICAL         IFLAG   !  true iff interp (instead of copy)
      LOGICAL         SFLAG   !  true iff controlled by synch file

      LOGICAL         EFLAG
      CHARACTER*256   MESG

      INTEGER         LDEV        !  log-device
      INTEGER         ISTAT       !  allocation-status

      LOGICAL         AFLAG, BFLAG, CFLAG, XFLAG, YFLAG

      INTEGER         C, R, V, N, NSIZE  !  loop counters

      INTEGER         NCOLS1      ! number of grid columns
      INTEGER         NROWS1      ! number of grid rows
      INTEGER         NLAYS1      ! number of layers
      INTEGER         GDTYP1      ! grid type:  1=LAT-LON, 2=UTM, ...
      REAL*8          P_ALP1      ! first, second, third map
      REAL*8          P_BET1      ! projection descriptive
      REAL*8          P_GAM1      ! parameters.
      REAL*8          XCENT1      ! lon for coord-system X=0
      REAL*8          YCENT1      ! lat for coord-system Y=0
      REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
      REAL*8          YORIG1      ! Y-coordinate origin of grid
      REAL*8          XCELL1      ! X-coordinate cell dimension
      REAL*8          YCELL1      ! Y-coordinate cell dimension

      INTEGER         NCOLS2       ! number of grid columns
      INTEGER         NROWS2       ! number of grid rows
      INTEGER         GDTYP2       ! grid type:  1=LAT-LON, 2=UTM, ...
      REAL*8          P_ALP2       ! first, second, third map
      REAL*8          P_BET2       ! projection descriptive
      REAL*8          P_GAM2       ! parameters.
      REAL*8          XCENT2       ! lon for coord-system X=0
      REAL*8          YCENT2       ! lat for coord-system Y=0
      REAL*8          XORIG2       ! X-coordinate origin of grid (map units)
      REAL*8          YORIG2       ! Y-coordinate origin of grid
      REAL*8          XCELL2       ! X-coordinate cell dimension
      REAL*8          YCELL2       ! Y-coordinate cell dimension

      INTEGER         JDATE, JTIME, TSTEP
      INTEGER         EDATE, ETIME, TSECS, NRECS

      REAL,    ALLOCATABLE :: BUF1( :,: )
      REAL,    ALLOCATABLE :: BUF2( :,: )
      INTEGER, ALLOCATABLE ::  IX2( : )
      REAL,    ALLOCATABLE ::  PX2( : )
      REAL,    ALLOCATABLE ::  PY2( : )
      INTEGER, ALLOCATABLE :: IBUF( :,: )
      REAL(8), ALLOCATABLE :: DBUF( :,: )

C MSK CHANGE START 19.09.2019 (J. Bieser)
        INTEGER         IOS
        CHARACTER*16    STARTDATE
C MSK CHANGE END

      !!***********************************************************************
      !!   begin body of program M3CPLE


        LDEV  = INIT3()
        EFLAG = .FALSE.

        WRITE( *, '( 5X, A )' ) BLANK, BAR, BLANK,    
     & 'If you wish to copy time steps, instead of interpolate them,',                          
     & 'Program M3CPLE to read all variables in each time step in',
     & 'the specified time step sequence from the specified input',
     & 'file, optionally under the control of the specified',
     & 'synchronization file, copy or interpolate them to the output',
     & 'grid, and write them to the specified output file.',        
     & '',                                                                        
     & 'THE PROGRAM WILL PROMPT YOU for the logical names of the input',
     & 'data file, the input synch file, and the output file, the time',
     & 'step sequence, and the GRIDDESC name of the output grid.',                                   
     & '',                                                                        
     & 'Default responses are indicated in square brackets [LIKE THIS]',
     & 'and may be accepted by hitting the RETURN key.',                             
     & '',                                                                      
     & 'If you wish to copy time steps, instead of interpolate them,',           
     & 'respond "SAME" to the prompt for output grid name.',                     
     & '',                                                                       
     & 'PRECONDITIONS REQUIRED:',                                                
     & '',                                                                       
     & '    setenv <input data  file>    <path-name>',                           
     & '    setenv <input synch file>    <path-name, or "NONE">',                
     & '    setenv GRIDDESC              <path-name> (if interp)',               
     & '    setenv IOAPI_ISPH            <USGS spheroid, or REARTH>',           
     & '',                                                                      
     & '    Time step sequence is valid for both input files',                  
     & '',                                                                      
     & '    For COPY:  file type must be GRIDDED, BOUNDARY, or CUSTOM.',        
     & '    (respond "SAME" to the prompt for output grid name).',               
     & ' ',                                                                        
     & '    For INTERPOLATION:  the files must be of type GRIDDED,',              
     & '    all of the variables must be of type REAL, and',                      
     & '    the output grid should have a finer resolution than the',             
     & '    input grid (else you should use an aggregation program',             
     & '    like "mtxcple" instead of an interpolation program).',                
     & ' ',                                                                        
     & 'See URL',                                                                 
     & ' ',                                                                        
     & 'https://www.cmascenter.org/ioapi/documentation/3.1/',
     & 'html/AA.html#tools',
     &  '',                                                  
     & 'Program copyright (C) 1992-2002 MCNC',                                   
     & '(C) 1995-2013, 2018- Carlie J. Coats, Jr.',                               
     & '(C) 2003-2010 Baron Advanced Meteorological Systems, LLC., and',          
     & '(C) 2014-2016 UNC Institute for the Environment.',                        
     & 'Released under Version 2 of the GNU General Public License.',             
     & 'See enclosed GPL.txt, or URL',                                            
     & ''  ,                                                                      
     & '    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',              
     & ''  ,                                                                      
     & 'Comments and questions are welcome and can be sent to'  ,                 
     & '',                                                                        
     & '    Carlie J. Coats, Jr.    carlie@jyarborough.com',                      
     & 'or',                                                                      
     & '    UNC Institute for the Environment',                                        
     & '    100 Europa Dr., Suite 490 Rm 405',                                    
     & '    Campus Box 1105',                                                     
     & '    Chapel Hill, NC 27599-1105',                                          
     & ' ',                                                                        
     & 'Program version: ',                                                       
     & '$Id: m3cple.f90 108 2018-09-07 18:59:37Z coats $',
     & ' '


        IF ( .NOT. GETVAL( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Program terminated at user request', 2 )
        END IF


        !!...............  Open and get description for optional synch file

        MESG  = 'Enter name for input synch file, or "NONE"'
        SNAME = PROMPTMFILE( MESG, FSREAD3, 'NONE', PNAME )
        SFLAG = ( SNAME .NE. 'NONE' )

        IF ( SFLAG ) THEN

            IF ( DESC3( SNAME ) ) THEN
                SVBLE  = VNAME3D( 1 )
            ELSE
                MESG = 'Could not get file description for ' // SNAME
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

        END IF          !  if synch-flag option taken


        !!...............  Open and get description for input data file

        MESG  = 'Enter name for input data file'
        FNAME = PROMPTMFILE( MESG, FSREAD3, 'INFILE', PNAME )

        IF ( DESC3( FNAME ) ) THEN
            NCOLS1 = NCOLS3D
            NROWS1 = NROWS3D
            NLAYS1 = NLAYS3D
            GDTYP1 = GDTYP3D
            P_ALP1 = P_ALP3D
            P_BET1 = P_BET3D
            P_GAM1 = P_GAM3D
            XCENT1 = XCENT3D
            YCENT1 = YCENT3D
            XORIG1 = XORIG3D
            YORIG1 = YORIG3D
            XCELL1 = XCELL3D
            YCELL1 = YCELL3D

            IF ( ISCMAQ( FNAME ) ) THEN
                CFLAG = ENVYN( 'COPY_META', 
     &            'Copy CMAQ metadata to output file?', .TRUE., ISTAT )
                IF ( ISTAT .GT. 0 ) THEN
                    EFLAG = .TRUE.
                 CALL M3MESG( 'Bad environment variable "COPY_META"')
                ELSE IF ( .NOT.CFLAG ) THEN
                    CONTINUE
                ELSE IF ( .NOT.GETCMAQ( FNAME ) ) THEN
                    EFLAG = .TRUE.
            CALL M3MESG( 'Could not get CMAQ metadata for '// FNAME )
                END IF
            END IF

        ELSE
            MESG = 'Could not get file description for ' // FNAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


        !!...............  Get CMAQ metadata
       


        !!...............  Get output grid description, time step sequence

        WRITE( *, '( 5X, A )' ) BLANK,     
     & 'If you wish to copy time steps (keeping the output grid',
     & 'the same) instead of interpolating them to a new output',
     & 'grid, respond [SAME] to the prompt for output grid name.',                             
     & 'Otherwise, give the GRIDDESC name for the output grid.',
     &  ' '

         CALL GETSTR( 'Enter output grid name, or "SAME"',
     & 'SAME', GNAME )

         IF ( TSTEP3D .EQ. 0 ) THEN
             JDATE = 0
             JTIME = 0
             TSTEP = 0
            NRECS = 1
            GO TO  11
         END IF

! MSK START CHANGE 19.08.2019
! MSK         JDATE = GETVAL( SDATE3D, 9999999, SDATE3D,
! MSK     & 'Enter STARTING DATE for time step sequence' )

        CALL ENVSTR( 'NEWDATE','Enter STARTING DATE, or "SAME"',
     &               'SAME', STARTDATE, IOS )

C now read startdate into JDATE
        READ(STARTDATE(1:7),'(I7)') JDATE

	WRITE( MESG,* ) 'Start Date: ', JDATE
	CALL M3MESG( MESG )

C MSK END CHANGE

         JTIME = GETVAL(       0, 9999999, STIME3D,
     & 'Enter STARTING TIME for time step sequence' )

         TSTEP = GETVAL( TSTEP3D, 9999999, TSTEP3D,
     & 'Enter   TIME STEP   for time step sequence' )

         CALL LASTTIME( SDATE3D,STIME3D,TSTEP3D, MXREC3D, EDATE,ETIME )
         N     = CURREC( EDATE, ETIME, JDATE, JTIME, TSTEP, C, R )
         NRECS = GETVAL( 1, 9999999, N, 
     & 'Enter     NRECS     for time step sequence' )

         SDATE3D = JDATE
         STIME3D = JTIME
         TSTEP3D = TSTEP

11       CONTINUE        !  target of "if tstep3d is zero"


    !!...............  Setup for mode of operation:  copy or interpolate:

         CNAME = GNAME
         CALL LUSTR( CNAME )
         IF ( ( CNAME .EQ. 'SAME' ) ) THEN   !  set up for copy

           IFLAG = .FALSE.

           IF ( FTYPE3D .EQ. GRDDED3 ) THEN
               NSIZE = NCOLS3D * NROWS3D
           ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
               NSIZE = 2*NTHIK3D*( NCOLS3D + NROWS3D + 2*NTHIK3D )
           ELSE IF ( FTYPE3D .EQ. CUSTOM3 ) THEN
               NSIZE = NCOLS3D
           ELSE
               MESG = 'Cannot copy--' // 
     & 'file type not GRIDDED, BOUNDARY, or CUSTOM'
               CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
               EFLAG = .TRUE.
           END IF

           ALLOCATE( BUF1( NSIZE, NLAYS3D ),  
     &                  IBUF( NSIZE, NLAYS3D ),  
     &                  DBUF( NSIZE, NLAYS3D ),   STAT = ISTAT )
           IF ( ISTAT .NE. 0 ) THEN
           WRITE( MESG,'( A, I10 )') 'Buffer allocation failed: STAT=',
     &              ISTAT
               CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
           END IF

         ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN

           MESG = 'File type not GRIDDED--cannot interpolate'
           CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

         ELSE    !  set up for interpolate:

           IFLAG = .TRUE.

           GDNAM3D = GNAME
           IF ( DSCGRID( GNAME, CNAME, GDTYP3D,                     
     &                P_ALP3D, P_BET3D, P_GAM3D, XCENT3D, YCENT3D,
     &                XORIG3D, YORIG3D, XCELL3D, YCELL3D,        
     &                NCOLS3D, NROWS3D, NTHIK3D ) ) THEN

            NCOLS2 = NCOLS3D
            NROWS2 = NROWS3D
            GDTYP2 = GDTYP3D
            P_ALP2 = P_ALP3D
            P_BET2 = P_BET3D
            P_GAM2 = P_GAM3D
            XCENT2 = XCENT3D
            YCENT2 = YCENT3D
            XORIG2 = XORIG3D
            YORIG2 = YORIG3D
            XCELL2 = XCELL3D
            YCELL2 = YCELL3D

            CALL M3MESG( BAR )
            CALL M3MESG( 'Input grid parameters' )
            WRITE( MESG, '( A, I10 )' ) 'NCOLS=', NCOLS1
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'NROWS=', NROWS1
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'GDTYP=', GDTYP1
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'P_ALP', P_ALP1
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'P_BET', P_BET1
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'P_GAM', P_GAM1
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'XCENT', XCENT1
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'YCENT', YCENT1
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'XORIG', XORIG1
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'YORIG', YORIG1
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'XCELL', XCELL1
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'YCELL', YCELL1
            CALL M3MESG( MESG )
            CALL M3MESG( BAR )
            CALL M3MESG( 'Output grid parameters' )
            WRITE( MESG, '( A, I10 )' ) 'NCOLS=', NCOLS2
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'NROWS=', NROWS2
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I10 )' ) 'GDTYP=', GDTYP2
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'P_ALP', P_ALP2
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'P_BET', P_BET2
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'P_GAM', P_GAM2
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'XCENT', XCENT2
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'YCENT', YCENT2
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'XORIG', XORIG2
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'YORIG', YORIG2
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'XCELL', XCELL2
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, 1PE24.16 )' ) 'YCELL', YCELL2
            CALL M3MESG( MESG )
            CALL M3MESG( BAR )

          ELSE

              MESG   = '"' // TRIM( GNAME ) //
     & '" not found in GRIDDESC file'
              CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

          END IF


    !!...............  Allocate buffers; compute re-gridding matrix

          ALLOCATE( BUF1( NCOLS1*NROWS1, NLAYS1 ), 
     &                  BUF2( NCOLS2*NROWS2, NLAYS1 ), 
     &                  IX2 ( NCOLS2*NROWS2 ),         
     &                  PX2 ( NCOLS2*NROWS2 ),         
     &                  PY2 ( NCOLS2*NROWS2 ),  STAT = ISTAT )

          IF ( ISTAT .NE. 0 ) THEN
              WRITE( MESG, 
     & '( A, I10)' ) 'Buffer allocation failed:  STAT=', ISTAT
              CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
          END IF

          CALL GRID2INDX( GDTYP1,P_ALP1,P_BET1,P_GAM1,XCENT1,YCENT1, 
     &                        GDTYP2,P_ALP2,P_BET2,P_GAM2,XCENT2,YCENT2,
     &                        NCOLS1,NROWS1,XORIG1,YORIG1,XCELL1,YCELL1,
     &                        NCOLS2,NROWS2,XORIG2,YORIG2,XCELL2,YCELL2,
     &                        IX2, PX2, PY2 )

         END IF      !  if gname = "SAME", or not


      !!...............  Open output file

        CALL GETSTR( 'Enter name for output data file',
     &  'OUTFILE', ONAME )

        IF ( .NOT. OPEN3( ONAME, FSUNKN3, PNAME ) ) THEN
          MESG = 'Could not open ' // ONAME
          CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


      !!...............  Process output time step sequence

        CALL NEXTIME( JDATE, JTIME, -TSTEP )

        DO  N = 1, NRECS

          CALL NEXTIME( JDATE, JTIME, TSTEP )

          IF ( SFLAG ) THEN           !!  Synch-file processing
              IF ( .NOT. CHECK3( SNAME, SVBLE, JDATE, JTIME ) ) THEN
                  EFLAG = .TRUE.
                  CALL M3MESG( 'ERROR:  synch-failure' )
                  CYCLE
              END IF
          END IF

          WRITE( MESG,
     & '( A, I7.7, A, I6.6 )' ) 'Processing  ', JDATE, ':', JTIME
          CALL M3MSG2( ' ' )
          CALL M3MSG2( MESG )

          IF ( IFLAG ) THEN   !  bilin-interpolate vs. copy

              DO  V = 1, NVARS3D

                IF ( VTYPE3D( V ) .NE. M3REAL ) THEN
                    EFLAG = .TRUE.
                    MESG  = 'ERROR:  variable "' // TRIM( VNAME3D( V ) ) // '" not of type REAL'
                    CALL M3MESG( MESG )
                    CYCLE
                ELSE IF ( .NOT. READ3( FNAME, VNAME3D( V ), ALLAYS3, 
     &           JDATE, JTIME, BUF1 ) ) THEN
                     EFLAG = .TRUE.
                     CALL M3MESG( 'ERROR:  read-failure' )
                     CYCLE
                END IF

                CALL INDXMULT( NCOLS2*NROWS2, NLAYS1,     
     &                               NCOLS1, NROWS1, IX2, PX2, PY2,
     &                               BUF2, BUF1 )

                IF ( .NOT.WRITE3( ONAME, VNAME3D(V), JDATE,
     &           JTIME, BUF2 ) ) THEN
                    EFLAG = .TRUE.
                    CALL M3MESG( 'ERROR:  write-failure' )
                    CYCLE
                END IF

             END DO      !  end loop on variables

         ELSE        !  else no interpolation:  copy only.

            DO  V = 1, NVARS3D

                IF ( VTYPE3D( V ) .EQ. M3REAL ) THEN

                    IF ( .NOT. READ3( FNAME, VNAME3D( V ),
     &         ALLAYS3, JDATE, JTIME, BUF1 ) ) THEN
                        EFLAG = .TRUE.
                        CALL M3MESG( 'ERROR:  read-failure' )
                    ELSE IF ( .NOT.WRITE3( ONAME, 
     &         VNAME3D( V ), JDATE, JTIME, BUF1 ) ) THEN
                        EFLAG = .TRUE.
                        CALL M3MESG( 'ERROR:  write-failure' )
                    END IF

                ELSE IF ( VTYPE3D( V ) .EQ. M3INT ) THEN

                    IF ( .NOT. READ3( FNAME, VNAME3D( V ),
     &                ALLAYS3, JDATE, JTIME, IBUF ) ) THEN
                        EFLAG = .TRUE.
                        CALL M3MESG( 'ERROR:  read-failure' )
                    ELSE IF ( .NOT.WRITE3( ONAME, VNAME3D( V ),
     &                JDATE, JTIME, IBUF ) ) THEN
                        EFLAG = .TRUE.
                        CALL M3MESG( 'ERROR:  write-failure' )
                    END IF

                ELSE IF ( VTYPE3D( V ) .EQ. M3DBLE ) THEN

                  IF ( .NOT. READ3( FNAME, VNAME3D( V ),
     &                ALLAYS3, JDATE, JTIME, DBUF ) ) THEN
                        EFLAG = .TRUE.
                        CALL M3MESG( 'ERROR:  read-failure' )
                    ELSE IF ( .NOT.WRITE3( ONAME, VNAME3D( V ),
     &               JDATE, JTIME, DBUF ) ) THEN
                        EFLAG = .TRUE.
                        CALL M3MESG( 'ERROR:  write-failure' )
                    END IF

                END IF

            END DO      !  end loop on variables

         END IF      !  if iflag, or not

       END DO          !  end loop on output time steps


       IF ( EFLAG ) THEN
         MESG  = 'Failure in program'
         ISTAT = 2
       ELSE
         MESG  = 'Success in program'
         ISTAT = 0
       END IF

       CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


       END PROGRAM M3CPLE

