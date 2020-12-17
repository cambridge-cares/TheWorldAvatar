
LOGICAL FUNCTION CHKFIL3( FID )  RESULT( CHKFLAG )

    !!***********************************************************************
    !!EDSS/Models-3 I/O API.
    !!Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !!(C) 2003-2011 Baron Advanced Meteorological Systems, and 
    !!(C) 2015 UNC Institute for the Environment
    !!Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !!See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !! function body starts at line  96
    !!
    !! FUNCTION:  Check consistency between file description in FDESC3
    !!            structures and STATE3 tables for new version of OPEN3()
    !!            Report inconsistencies to the log.
    !!
    !! RETURN VALUE:  TRUE iff consistent
    !!
    !! PRECONDITIONS REQUIRED:  call set up correctly by OPEN3()
    !!
    !! SUBROUTINES AND FUNCTIONS CALLED:  TRIMLEN
    !!
    !! REVISION  HISTORY:
    !!      prototype 9/94 by CJC for new version of OPEN3()
    !!      Bug fix  10/96 by CJC to *ERR() -- avoid overflow for BADVAL3 args.
    !!      Modified 2/97 by CJC:  Additional check for legality of variable-names
    !!      revised  6/99 by CJC:  OpenMP thread-safety
    !!      revised  8/2000 by CJC:  format-bug at lines 431-432
    !!      revised 10/2000 by CJC:  weakened the SDATE:STIME checking --
    !!              SDATE3D:STIME3D must be consistent with the timestep
    !!              sequence of the file.
    !!      Modified  2/2004 by CJC for I/O API version 3:  support for
    !!      native-binary BINFIL3 file type.
    !!      Modified  8/2007 by CJC:  support same-set/different-order
    !!      variables-lists.
    !!      Modified 11/2007 by CJC:  Revert from 8/2007:  same-set/different-order
    !!      is bogus
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!
    !!      Modified 02/2015 by CJC for I/O API 3.2: USE M3UTILIO
    !!
    !!      Modified 08/2015 by CJC for PnetCDF distributed I/O; USE NCF_MODULE;
    !!      free F90 source format
    !!***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !!...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: FID             !  subscript for STATE3 arrays


    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: CKNAME


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    LOGICAL     OKFLAG, MPIFLAG
    INTEGER     IREC              !  timestep record number
    INTEGER     IERR              !  netCDF error status return
    INTEGER     VAR, LVL, V, I    !  loop counters
    INTEGER     VGTYP             !  vertical coordinate type
    REAL        VGTOP             !  model-top (sigma only)
    REAL        VGLVLS( MXLAYS3 ) !  vertical level values

    !!.......   Statement functions for evaluating relative error: TRUE
    !!.......   iff P,Q (X,Y) are significantly different (compares
    !!.......   normalized difference against tolerance)

    LOGICAL         DBLERR, FLTERR
    REAL*8          P, Q
    REAL            X, Y

    DBLERR( P,Q ) = ( ( P .GT. AMISS3 ) .AND. ( Q .GT. AMISS3 ) .AND.     &
                      ( P - Q )**2 / ( P*P + Q*Q + 1.0D-5 ) .GT. 1.0D-12 )
        
    FLTERR( X,Y ) = ( ( X .GT. AMISS3 ) .AND. ( Y .GT. AMISS3 ) .AND.     &
                      ( X - Y )**2 / ( X*X + Y*Y + 1.0E-5 ) .GT. 1.0E-10 )
        
    !!.............................................................................
    !!  begin body of subroutine  CHKFIL3

!$OMP CRITICAL( S_LOGOUT )

        OKFLAG = .TRUE.
        MPIFLAG = .FALSE.

    !!...........   Check attributes valid for all file types:
    !!...........   FTYPE:  file type ID

    IF ( FTYPE3D       .EQ. GRDDED3 .AND.   &
         FTYPE3( FID ) .EQ. MPIGRD3 ) THEN
        MPIFLAG = .TRUE.
    ELSE IF ( FTYPE3D .NE. FTYPE3( FID ) ) THEN
        WRITE( LOGDEV,91020 )                                                   &
            'Inconsistent file attribute FTYPE for file '  // FLIST3( FID ),    &
            'Value from file:  ', FTYPE3( FID ), 'Value from caller:', FTYPE3D
        OKFLAG = .FALSE.
    ELSE IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN
        OKFLAG = .TRUE.
        GO TO  999
    END IF

    !!.......   NVARS:  number of variables

    IF ( NVARS3D .NE. NVARS3( FID ) ) THEN
        WRITE( LOGDEV,91020 )                                                   &
            'Inconsistent file attribute NVARS for file ' // FLIST3( FID ),     &
            'Value from file:  ', NVARS3( FID ), 'Value from caller:', NVARS3D
        OKFLAG = .FALSE.
    END IF 
     

    !!.......   Continue checks for data files -- TSTEP:  time step

    IF ( TSTEP3D .NE. TSTEP3( FID ) ) THEN
        WRITE( LOGDEV,91020 )                                                   &
            'Inconsistent file attribute TSTEP for file ' // FLIST3( FID ),     &
            'Value from file:  ', TSTEP3( FID ),'Value from caller:', TSTEP3D
        OKFLAG = .FALSE.
    END IF
        
    !!.......   SDATE:  starting date (Julian date YYYYDDD)
    !!.......   STIME:  starting time (HHMMSS)

    IF ( TSTEP3D .NE. 0 ) THEN
        IREC = JSTEP3( SDATE3D, STIME3D, SDATE3( FID ), STIME3( FID ), TSTEP3D )
        IF ( IREC .LT. 0 ) THEN
            WRITE( LOGDEV, 91021 )                                      &
            'Inconsistent SDATE:STIME for file ' // FLIST3( FID ),      &
            'Value from file:  ', SDATE3( FID ), ':', STIME3( FID ),    &
            'Value from caller:', SDATE3D,       ':', STIME3D
        OKFLAG = .FALSE.
        END IF
    END IF
      
    !!.......   NTHIK:  perimeter thickness (cells; boundary files only)

    IF( FTYPE3D .EQ. BNDARY3 ) THEN
        IF ( NTHIK3D .NE. NTHIK3( FID ) ) THEN
            WRITE( LOGDEV,91020 )                                                   &
                'Inconsistent file attribute NTHIK for file ' // FLIST3( FID ),     &
                'Value from file:  ', NTHIK3( FID ), 'Value from caller:', NTHIK3D
            OKFLAG = .FALSE.
        END IF
    END IF
      
    !!.......   NCOLS:  number of grid columns/profile levels (not used for IDDATA)

    IF ( FTYPE3D .NE. IDDATA3 ) THEN
        IF ( NCOLS3D .NE. NCOLS3( FID ) ) THEN
            WRITE( LOGDEV,91020 )                                                   &
                'Inconsistent file attribute NCOLS for file ' // FLIST3( FID ),     &
                'Value from file:  ', NCOLS3( FID ), 'Value from caller:', NCOLS3D
            OKFLAG = .FALSE.
        END IF
    END IF
      
    !!.......   NROWS:  number of grid rows/data sites.  Not used for CUSTOM.

    IF ( FTYPE3D .NE. CUSTOM3 ) THEN
        IF ( NROWS3D .NE. NROWS3( FID ) ) THEN
            WRITE( LOGDEV,91020 )                                                   &
                'Inconsistent file attribute NROWS for file ' // FLIST3( FID ),     &
                'Value from file:  ', NROWS3( FID ), 'Value from caller:', NROWS3D
            OKFLAG = .FALSE.
        END IF
    END IF
      
    !!.......   NLAYS:  number of layers

    IF ( NLAYS3D .NE. NLAYS3( FID ) ) THEN
        WRITE( LOGDEV,91020 )                                                   &
            'Inconsistent file attribute NLAYS for file ' // FLIST3( FID ),     &
            'Value from file:  ', NLAYS3( FID ), 'Value from caller:', NLAYS3D
        OKFLAG = .FALSE.
    END IF
      
    !!.......   GDTYP:  grid type ID (lat-lon, UTM, RADM, etc...)

    IF ( GDTYP3D .NE. GDTYP3( FID ) ) THEN
        WRITE( LOGDEV,91020 )                                                   &
            'Inconsistent file attribute GDTYP for file ' // FLIST3( FID ),     &
            'Value from file:  ', GDTYP3( FID ), 'Value from caller:', GDTYP3D
        OKFLAG = .FALSE.
    END IF

    !!.......   P_ALP:  first map-projection-description angle               
      
    IF ( DBLERR( P_ALP3D , P_ALP3( FID ) ) ) THEN
        WRITE( LOGDEV,91030 )                                                   &
            'Inconsistent file attribute P_ALP for file ' // FLIST3( FID ),     &
            'Value from file:  ', P_ALP3( FID ), 'Value from caller:', P_ALP3D
        OKFLAG = .FALSE.
    END IF
      
    !!.......   P_BET:  second map-projection-description angle              

    IF ( DBLERR( P_BET3D , P_BET3( FID ) ) ) THEN
        WRITE( LOGDEV,91030 )                                                   &
            'Inconsistent file attribute P_BET for file ' // FLIST3( FID ),     &
            'Value from file:  ', P_BET3( FID ), 'Value from caller:', P_BET3D
        OKFLAG = .FALSE.
    END IF
      
    !!.......   P_GAM:  third map-projection-description angle               

    IF ( DBLERR( P_GAM3D , P_GAM3( FID ) ) ) THEN
        WRITE( LOGDEV,91030 )                                                   &
            'Inconsistent file attribute P_GAM for file ' // FLIST3( FID ),     &
            'Value from file:  ', P_GAM3( FID ), 'Value from caller:', P_GAM3D
        OKFLAG = .FALSE.
    END IF
      
    !!.......   XCENT:  lon of coordinate-system (0,0) origin
      
    IF ( DBLERR( XCENT3D , XCENT3( FID ) ) ) THEN
        WRITE( LOGDEV,91030 )                                                   &
            'Inconsistent file attribute XCENT for file ' // FLIST3( FID ),     &
            'Value from file:  ', XCENT3( FID ), 'Value from caller:', XCENT3D
        OKFLAG = .FALSE.
    END IF
      
    !!.......   YCENT:  lat of coordinate-system (0,0) origin

    IF ( DBLERR( YCENT3D , YCENT3( FID ) ) ) THEN
        WRITE( LOGDEV,91030 )                                                   &
            'Inconsistent file attribute YCENT for file ' // FLIST3( FID ),     &
            'Value from file:  ', YCENT3( FID ), 'Value from caller:', YCENT3D
        OKFLAG = .FALSE.
    END IF
      
    !!.......   XORIG:  X-coord of grid origin
    !!.......   (in map units; see FDESC3.EXT for description)

    IF ( DBLERR( XORIG3D , XORIG3( FID ) ) ) THEN
        WRITE( LOGDEV,91030 )                                                   &
            'Inconsistent file attribute XORIG for file ' // FLIST3( FID ),     &
            'Value from file:  ', XORIG3( FID ), 'Value from caller:', XORIG3D
        OKFLAG = .FALSE.
    END IF
      
    !!.......   YORIG:  Y-coord of grid origin
    !!.......   (in map units; see FDESC3.EXT for description)

     IF ( DBLERR( YORIG3D , YORIG3( FID ) ) ) THEN
         WRITE( LOGDEV,91030 )                                                   &
             'Inconsistent file attribute YORIG for file ' // FLIST3( FID ),     &
             'Value from file:  ', YORIG3( FID ), 'Value from caller:', YORIG3D
         OKFLAG = .FALSE.
     END IF
      
    !!.......   XCELL:  cell width (X direction)
    !!.......   (in map units; see FDESC3.EXT for description)

    IF ( DBLERR( XCELL3D , XCELL3( FID ) ) ) THEN
        WRITE( LOGDEV,91030 )                                                   &
            'Inconsistent file attribute XCELL for file ' // FLIST3( FID ),     &
            'Value from file:  ', XCELL3( FID ), 'Value from caller:', XCELL3D
        OKFLAG = .FALSE.
    END IF
      
    !!.......   YCELL:  cell width (Y direction)
    !!.......   (in map units; see FDESC3.EXT for description)

    IF ( DBLERR( YCELL3D , YCELL3( FID ) ) ) THEN
        WRITE( LOGDEV,91030 )                                                   &
            'Inconsistent file attribute YCELL for file ' // FLIST3( FID ),     &
            'Value from file:  ', YCELL3( FID ), 'Value from caller:', YCELL3D
        OKFLAG = .FALSE.
    END IF
         
    !!.......   VGTYP:  grid type ID (lat-lon, UTM, RADM, etc...)

    IF ( CDFID3( FID ) .NE. VIRFIL3  .AND.           &
         CDFID3( FID ) .NE. BINFIL3 ) THEN

        IF ( NLAYS3D .GT. 1 ) THEN

            IF ( VGTYP3D .NE. VGTYP3( FID ) ) THEN
                WRITE( LOGDEV,91020 )                                                   &
                    'Inconsistent file attribute VGTYP for file ' // FLIST3( FID ),     &
                    'Value from file:  ', VGTYP3( FID ), 'Value from caller:', VGTYP3D
                OKFLAG = .FALSE.
            END IF

            IF ( FLTERR( VGTOP3D, VGTOP3( FID ) ) ) THEN
                WRITE( LOGDEV,91040 )                                                   &
                    'Inconsistent file attribute VGTOP for file ' // FLIST3( FID ),     &
                    'Value from file:  ', VGTOP3( FID ), 'Value from caller:', VGTOP3D
                OKFLAG = .FALSE.
            END IF
         
            !!...........   VGLVS( 1, ..., NLAYS3D+1 ):  vertical coordinate values
             
            DO  LVL = 1, MIN( NLAYS3D+1, MXLAYS3 )

                IF ( FLTERR( VGLVS3D( LVL ), VGLVS3( LVL,FID ) ) ) THEN

                    WRITE( LOGDEV,91040 )                                                   &
                        'Inconsistent attribute VGLVLS for file ' // FLIST3( FID ),         &
                        'Value from file:  ', VGLVS3( LVL,FID ),                            &
                        'Value from caller:', VGLVS3D( LVL ), 'Error at level    ', LVL
                    OKFLAG = .FALSE.

                END IF  !  if flterr()

            END DO    !  end loop on LVL

        END IF              !  if #( layers ) > 1

    END IF          !  if not BUFFERED or VIRTUAL file

    !!.......   GDNAM:  grid name

    IF ( GDNAM3D .NE. GDNAM3( FID ) ) THEN
        WRITE( LOGDEV,91010 )                                                   &
            'Inconsistent grid name for file '// FLIST3( FID ),                 &
            'Value from file:  ' // GDNAM3( FID ) , 'Value from caller:' // GDNAM3D
        OKFLAG = .FALSE.
    END IF
      
           
    !!.......   Variables-list for the file:

    DO  V = 1 , NVARS3( FID )

        IF ( .NOT. CKNAME( VNAME3D( V ) ) ) THEN
            OKFLAG = .FALSE.
            WRITE( LOGDEV,91010 ) 'Illegal variable name "' // VNAME3D( V ) // '" in file ' // FLIST3( FID )
        ELSE IF ( VNAME3D(V) .NE. VLIST3(V,FID) ) THEN
            OKFLAG = .FALSE.
            WRITE( LOGDEV,91010 )                                                   &
                'Inconsistent variable list for file '//FLIST3(FID),                &
                'Value from file:   '// VLIST3(V,FID), 'Value from caller: '// VNAME3D( V )
        ELSE IF ( VTYPE3( V,FID ) .NE. VTYPE3D( V ) ) THEN
            OKFLAG = .FALSE.
            WRITE( LOGDEV,91020 )                                                   &
                'Inconsistent type for variable ' // VNAME3D( V ) //                &
                ' from file ' // FLIST3( FID ),                                     &
                'Value from file:  ', VTYPE3( V,FID ), 'Value from caller:', VTYPE3D( V )
        END IF

    END DO      !!  end loop on V
           
999 CONTINUE
           
    CHKFLAG = OKFLAG

!$OMP END CRITICAL( S_LOGOUT )

    !!.......   Control only falls through to here if everything succeeds:

    RETURN
     
    !!******************  FORMAT  STATEMENTS   ******************************

    !!...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine CHKFIL3 <<<',    &
                  3 ( /5X , A , : ) , I5, // )

91020   FORMAT ( //5X , '>>> WARNING in subroutine CHKFIL3 <<<',    &
                  /5X , A, 2 ( /5X , A , I9, : ) )

91021   FORMAT ( //5X , '>>> WARNING in subroutine CHKFIL3 <<<',    &
                  /5X , A, 2 ( /5X , A , I9, A, I6.6, : ) )

91030   FORMAT ( //5X , '>>> WARNING in subroutine CHKFIL3 <<<',    &
                  /5X , A, 2 ( /5X , A , 1PD24.17 , : ) )

91040   FORMAT ( //5X , '>>> WARNING in subroutine CHKFIL3 <<<',    &
                  /5X , A, 2 ( /5X , A , 1PE12.5 , : ), /5X , A , I9 )

END FUNCTION CHKFIL3

 
