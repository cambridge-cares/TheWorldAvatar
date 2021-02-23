
        LOGICAL   FUNCTION CHECK3( FNAME, VNAME, JDATE, JTIME )

C***********************************************************************
C Version "$Id: check3.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2011 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and 
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  110
C
C  FUNCTION:  returns TRUE iff time step for (JDATE,JTIME) and variable
C       VNAME is available in file with logical name FNAME.  If FNAME 
C       is time independent or is a dictionary file, only VNAME is 
C       significant and the routine returns the availability of the
C       variable or file description for VNAME.
C
C  PRECONDITIONS REQUIRED:  
C       FNAME already opened by OPEN3().
C       VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C
C  REVISION  HISTORY:  
C       prototype 3/1992 by CJC
C
C       Modified  7/1994 by CJC to handle restart (circular-buffer) files
C
C       modified 10/1994 by CJC to work with variable-level-granularity
C       WRITE3() 
C
C       modified 19/1999 by C.Coats for thread-safety, A.Trayanov for
C       coupling-mode operation
C
C       Modified  1/2002 by CJC:  check TRIMLEN() of FNAME
C
C       Modified  3/2002 by CJC:  STATE3V changes; uses RDTFLAG()
C
C       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
C       associated with INIT3()
C
C       Modified 10/2003 by CJC for I/O API version 3:  support for
C       native-binary BINFIL3 file type; uses INTEGER NAME2FID
C
C       Modified 9/2004 by CJC for virtual INTERP bug-fix: change to
C       READ3V interface
C
C       Modified 11/2004 by CJC:  new "verbose-flag" argument to RDTFLAG
C
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C       Modified 03/2010 by CJC: eliminate unused NETCDF.EXT
C***********************************************************************

        IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
#ifdef IOAPICPL
        INCLUDE 'STATE3V.EXT'
#endif


C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) ::   FNAME   !  file name for query
        CHARACTER*(*), INTENT(IN   ) ::   VNAME   !  vble name for query
        INTEGER      , INTENT(IN   ) ::   JDATE   !  date (YYYYDDD) for query
        INTEGER      , INTENT(IN   ) ::   JTIME   !  time (HHMMSS) for query


C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: INDEX1
        INTEGER, EXTERNAL :: NAME2FID   !  fname~~> fid lookup
        LOGICAL, EXTERNAL :: RDTFLAG    !  check time step record availability
        
        EXTERNAL :: INITBLK3   !  block data: initialize I/O state


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         FID             !  subscript for STATE3 arrays
        INTEGER         STEP            !  time step record number
        INTEGER         VID             !  subscript for vble in state arrays
        INTEGER         VLEN            !  name lengths for file, vble
        LOGICAL         EFLAG
        CHARACTER*256   MESG

#ifdef IOAPICPL
        INTEGER       COUNT, SKIP, TYPE
        INTEGER       F1, F2
        LOGICAL       VFLAG
        REAL          BUFFER(1)
        LOGICAL       READ3V
        EXTERNAL      READ3V
#endif


C***********************************************************************
C   begin body of function  CHECK3

        FID   = NAME2FID( FNAME )
        EFLAG = ( FID .LE. 0 )

        VLEN  = LEN_TRIM( VNAME )

        IF ( VLEN .GT. NAMLEN3 ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '( A, I10 )'  )
     &          'Max vble name length 16; actual: ', VLEN
            CALL M3MSG2( MESG )
        END IF          !  if len( vname ) > 16
        
        IF ( EFLAG ) THEN
            MESG  = 'Invalid file "'// FNAME // 
     &             '" or variable "'// VNAME //'"'
            CALL M3WARN( 'CHECK3', JDATE, JTIME, MESG )
            CHECK3 = .FALSE.
            RETURN
        END IF
        

C.......   Find netCDF index for the file, and check time step availability:

        IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN
            
            VID  = INDEX1( VNAME, NVARS3( FID ), VLIST3( 1,FID ) )
            
        ELSE IF ( VNAME .EQ. ALLVAR3 ) THEN

            VID = ALLAYS3

        ELSE

            VID = INDEX1( VNAME, NVARS3( FID ), VLIST3( 1,FID ) )
            IF ( VID .EQ. 0 ) THEN
                CHECK3 = .FALSE.
                MESG = 'Variable "' // VNAME //
     &                 '" not in file "' // FNAME
                CALL M3WARN( 'CHECK3', JDATE, JTIME, MESG )
                RETURN
            END IF

        END IF              !  if FID OK; else if "all variables", or not
         
        
        IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN     !  virtual "file"
           
#ifdef IOAPICPL
            IF ( TSTEP3( FID ) .NE. 0 ) THEN
                F1    = JDATE
                F2    = JTIME
            ELSE
                F1    = 0
                F2    = 0
            END IF

            COUNT = 0
            SKIP  = 0
            IF ( VID .EQ. ALLAYS3 ) THEN
                EFLAG = .TRUE.
                DO VID = 1, NVARS3(FID)
                    VFLAG = READ3V( FID, VID, SKIP, COUNT,
     &                              F1, F2, BUFFER )
                    EFLAG = ( EFLAG .AND. VFLAG )
                END DO
                CHECK3 = EFLAG
            ELSE
                CHECK3 = READ3V( FID, VID, SKIP, COUNT,
     $                           F1, F2, BUFFER )
            END IF
#endif
#ifndef IOAPICPL
            CALL M3WARN( 'CHECK3', JDATE, JTIME, 
     &                   'Coupling mode not active' )
            CHECK3 = .FALSE.
#endif

        ELSE

            CHECK3 = RDTFLAG( FID, VID, JDATE, JTIME, STEP, .FALSE. )

        END IF  

        RETURN

        END FUNCTION CHECK3

