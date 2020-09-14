
#ifdef IOAPICPL
        LOGICAL FUNCTION UPDTVIR3( FID )
     &                     RESULT( UPDFLAG )

C***********************************************************************
C Version "$Id: updtvir3.F 1 2017-06-10 18:05:20Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2013 Baron Advanced Meteorological Systems,
C (C) 2007-2013 Carlie J. Coats, Jr., and
C (C) 2014 UNC Institute for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line 63
C
C  FUNCTION:  Updates STATE3 common variables for a "VIRTUAL "file" FNAME 
C  using info stored in the FDESC3 common.
C
C  PRECONDITIONS REQUIRED:  File does not yet exist.  Should only be
C                           called from OPEN3().  
C                           "File" must be one of types GRDDED3,
C                           BNDARY3, or CUSTOM3.
C
C  SUBROUTINES AND FUNCTIONS CALLED:  
C
C  REVISION  HISTORY:
C       prototype  7/1994 by CJC
C       Modified  10/1996 by CJC:  new file type TSERIES3 for hydrology work.
C       Modified  03/2010 by CJC: F9x changes for I/O API v3.1
C       Modified  02/2015 by CJC for I/O API 3.2: USE M3UTILIO
C***********************************************************************

        USE M3UTILIO

       IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'STATE3.EXT'
        INCLUDE 'STATE3V.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT(IN   ) :: FID             !  index into STATE3 arrays


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         VSIZE           !  size for one variable
        INTEGER         VAR     !  loop counter
        CHARACTER*256   MESG

C.............................................................................
C   begin body of subroutine  UPDTVIR3

C.......   Compute size for a one-variable/one-layer buffer for supported
C.......   "file" types, or error return:
        
        IF ( FTYPE3D .EQ. CUSTOM3 ) THEN        !  other dimensions not known
            VSIZE = NCOLS3D
        ELSE IF ( FTYPE3D .EQ. GRDDED3 ) THEN
            VSIZE = NCOLS3D * NROWS3D
        ELSE IF ( FTYPE3D .EQ. TSRIES3 ) THEN
            VSIZE = NCOLS3D * NROWS3D
        ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
            VSIZE = 2 * NTHIK3D
            VSIZE = IABS( VSIZE ) * ( NCOLS3D + NROWS3D + VSIZE )
        ELSE    !  illegal file type

           MESG = 'VIRTUAL-file creation error for file ' //
     $            FLIST3( FID ) // CHAR(10) //
     $            '     ILLEGAL VALUE FOR FILE TYPE.'
            CALL M3WARN( 'UPDTVIR3', SDATE3D, STIME3D, MESG )
            UPDFLAG = .FALSE.
            RETURN

        END IF
        
        BSIZE3( FID ) = VSIZE
        CDFID3( FID ) = VIRFIL3
        
C...........   Set attributes valid for all file types:
C...........   FTYPE:  file type ID

        FTYPE3( FID ) = FTYPE3D
        NVARS3( FID ) = NVARS3D
        SDATE3( FID ) = SDATE3D
        STIME3( FID ) = STIME3D
        TSTEP3( FID ) = TSTEP3D
        MXREC3( FID ) = 0
        TINDX3( FID ) = IMISS3

C...........   Set grid and coordinate system parameters
        
        NTHIK3( FID ) = NTHIK3D
        NCOLS3( FID ) = NCOLS3D
        NROWS3( FID ) = NROWS3D
        NLAYS3( FID ) = NLAYS3D
        NVARS3( FID ) = NVARS3D
        GDTYP3( FID ) = GDTYP3D
        P_ALP3( FID ) = P_ALP3D
        P_BET3( FID ) = P_BET3D
        P_GAM3( FID ) = P_GAM3D
        XCENT3( FID ) = XCENT3D
        YCENT3( FID ) = YCENT3D
        XORIG3( FID ) = XORIG3D
        YORIG3( FID ) = YORIG3D
        XCELL3( FID ) = XCELL3D
        YCELL3( FID ) = YCELL3D
        GDNAM3( FID ) = GDNAM3D

C.......   Define all the Models-3 variables for this file:

        DO 111  VAR = 1 , NVARS3D
            VINDX3( VAR,FID ) = IMISS3
            VTYPE3( VAR,FID ) = VTYPE3D( VAR )
            ILAST3( VAR,FID ) = IMISS3
            LDATE3( VAR,FID ) = IMISS3
            LTIME3( VAR,FID ) = IMISS3
            NDATE3( VAR,FID ) = IMISS3
            NTIME3( VAR,FID ) = IMISS3
            VLIST3( VAR,FID ) = VNAME3D( VAR )
111     CONTINUE

        UPDFLAG = .TRUE.

        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

91010   FORMAT ( //5X , '>>> WARNING in subroutine UPDTVIR3 <<<',
     &            2 ( /5X , A , : ) , I5, // )


        END FUNCTION UPDTVIR3

#endif
