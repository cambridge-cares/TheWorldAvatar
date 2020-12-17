
        LOGICAL FUNCTION  XTBUF3( FID, VID, 
     &                            LAY0, LAY1, ROW0, ROW1, COL0, COL1,
     &                            JDATE, JTIME,
     &                            BUFFER )
     &                    RESULT( XTFLAG )

C***********************************************************************
C Version "$Id: xtbuf3.f 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2010 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and 
C (C) 2015 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  87
C
C  FUNCTION:  reads data from Models-3 BUFFERED "file" with M3 file
C       index FID for variable with index VID and indicated col-row-layer
C       window for the date and time JDATE (coded YYYYDDD) JTIME (HHMMSS).
C       For time-independent files, JDATE:JTIME are ignored.
C       If VNAME is 'ALL', reads all variables; if LAYER is -1,
C       reads all layers.
C
C  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
C
C  PRECONDITIONS REQUIRED:
C       FNAME is a Models-3 BUFFERED "file" already opened by OPEN3().
C       Should only be called via XTRACT3().
C       For ALLLVARS3 xtracts, all variables must be of type M3REAL
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       JSTEP3, BUFXTR3, BUFXTR3D, BUFXTR3I
C
C  REVISION  HISTORY:  
C       Prototype 7/1994 by CJC
C
C	Modified 10/1994 by CJC to work with XTRACT3() having granularity
C	at the level of individual variables.
C
C       Modified 5/2002 to support types other than REAL
C
C       Modified 03/2010 by CJC
C
C       Modified 02/2015 by CJC for I/O API 3.2: Support for INTEGER*8,
C       USE M3UTILIO
C***********************************************************************

        USE M3UTILIO

        IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'STATE3.EXT'

C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID             !  file     id for STATE3 arrays
        INTEGER, INTENT(IN   ) :: VID             !  variable id  "     "     "
        INTEGER, INTENT(IN   ) :: LAY0            !  lower layer  bound
        INTEGER, INTENT(IN   ) :: LAY1            !  upper layer  bound
        INTEGER, INTENT(IN   ) :: ROW0            !  lower row    bound
        INTEGER, INTENT(IN   ) :: ROW1            !  upper row    bound
        INTEGER, INTENT(IN   ) :: COL0            !  lower column bound
        INTEGER, INTENT(IN   ) :: COL1            !  upper column bound
        INTEGER, INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
        INTEGER, INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
        REAL   , INTENT(  OUT) :: BUFFER(*)       !  output buffer array

C...........   EXTERNAL FUNCTIONS and their descriptions:

        INTEGER, EXTERNAL :: BUFXTR3, BUFXTR3D, BUFXTR3I    !  from bufint3.c


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       VAR, STEP       !  loop counter (over variables)
        CHARACTER*256 MESG


C***********************************************************************
C   begin body of function  XTBUF3

        IF ( VID .GT. 0 ) THEN		!  xtract on just this variable
        
            IF ( TSTEP3( FID ) .EQ. 0 ) THEN
                   
                IF( LDATE3( VID,FID ) .EQ. 0 ) THEN
                    STEP = ILAST3( VID,FID )
                ELSE
                    XTFLAG = .FALSE.
                    RETURN
                END IF
                
            ELSE IF ( JDATE .EQ. LDATE3( VID,FID ) .AND. 
     &                JTIME .EQ. LTIME3( VID,FID ) ) THEN
                
                STEP = ILAST3( VID,FID )
                
            ELSE IF ( JDATE .EQ. NDATE3( VID,FID ) .AND. 
     &                JTIME .EQ. NTIME3( VID,FID ) ) THEN
                
                STEP = 1 - ILAST3( VID,FID )        !  formula swaps 0 and 1
                
            ELSE
                
                WRITE( MESG, '( 4A )' )
     &              'Date and time not available for ',
     &              VLIST3( VID,FID ), ' in ', FLIST3( FID )
                CALL M3WARN( 'XTRACT3/XTBUF3', JDATE, JTIME, MESG )
                
                XTFLAG = .FALSE.
                RETURN
                
            END IF
                
            IF ( VTYPE3( VID,FID ) .EQ. M3REAL ) THEN
                XTFLAG = ( 0 .NE. BUFXTR3( FID, VID, 
     &                   LAY0, LAY1, ROW0, ROW1, COL0, COL1,
     &                   NLAYS3( FID ), NROWS3( FID ), NCOLS3( FID ),
     &                   STEP, BUFFER ) )
            ELSE IF ( VTYPE3( VID,FID ) .EQ. M3INT ) THEN
                XTFLAG = ( 0 .NE. BUFXTR3I( FID, VID, 
     &                   LAY0, LAY1, ROW0, ROW1, COL0, COL1,
     &                   NLAYS3( FID ), NROWS3( FID ), NCOLS3( FID ),
     &                   STEP, BUFFER ) )
            ELSE IF ( VTYPE3( VID,FID ) .EQ. M3DBLE ) THEN
                XTFLAG = ( 0 .NE. BUFXTR3D( FID, VID, 
     &                   LAY0, LAY1, ROW0, ROW1, COL0, COL1,
     &                   NLAYS3( FID ), NROWS3( FID ), NCOLS3( FID ),
     &                   STEP, BUFFER ) )
            ELSE IF ( VTYPE3( VID,FID ) .EQ. M3INT8 ) THEN
                XTFLAG = ( 0 .NE. BUFXTR3D( FID, VID, 
     &                   LAY0, LAY1, ROW0, ROW1, COL0, COL1,
     &                   NLAYS3( FID ), NROWS3( FID ), NCOLS3( FID ),
     &                   STEP, BUFFER ) )
            END IF
                
        ELSE				!  xtract on all variables
        
            DO  11  VAR = 1, NVARS3( FID )
        
                IF ( VTYPE3( VAR,FID ) .NE. M3REAL ) THEN
                    
                    MESG = 'ALLVAR3 nonREAL types not supported'
                    CALL M3WARN( 'XTRACT3/XTBUF3', JDATE, JTIME, MESG )
                    XTFLAG = .FALSE.
                    RETURN
                    
                ELSE IF ( TSTEP3( FID ) .EQ. 0 ) THEN
                       
                    IF( LDATE3( VAR,FID ) .EQ. 0 ) THEN
                        STEP = ILAST3( VAR,FID )
                    ELSE
                        XTFLAG = .FALSE.
                        RETURN
                    END IF
                    
                ELSE IF ( JDATE .EQ. LDATE3( VAR,FID ) .AND. 
     &                    JTIME .EQ. LTIME3( VAR,FID ) ) THEN
                    
                    STEP = ILAST3( VAR,FID )
                    
                ELSE IF ( JDATE .EQ. NDATE3( VAR,FID ) .AND. 
     &                    JTIME .EQ. NTIME3( VAR,FID ) ) THEN
                    
                    STEP = 1 - ILAST3( VAR,FID )        !  formula swaps 0 and 1
                    
                ELSE
                    
                    WRITE( MESG, '( 4A )' )
     &                  'Date and time not available for ',
     &                   VLIST3( VAR,FID ), ' in ', FLIST3( FID )
                    CALL M3WARN( 'XTRACT3/XTBUF3', JDATE, JTIME, MESG )
                    XTFLAG = .FALSE.
                    RETURN
                    
                END IF
                
                IF ( 0 .EQ. BUFXTR3( FID, VAR, 
     &                               LAY0, LAY1, ROW0, ROW1, 
     &                               COL0, COL1, NLAYS3( FID ), 
     &                               NROWS3( FID ), NCOLS3( FID ), 
     &                               STEP, BUFFER ) ) THEN
                        XTFLAG = .FALSE.
                        RETURN
                END IF		!  if bufxtr3() failed.

11          CONTINUE
                
        END IF		!  if vid > 0, or not
        
        RETURN
        END FUNCTION  XTBUF3

