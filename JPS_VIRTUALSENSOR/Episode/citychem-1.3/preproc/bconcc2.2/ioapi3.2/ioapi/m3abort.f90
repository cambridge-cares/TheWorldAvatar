
SUBROUTINE  M3ABORT( FNAME, FNUM, IERR, MESSAGE )

    !!***********************************************************************
    !! Version "$Id: m3abort.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 I/O API.
    !! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    !! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    !! (C) 2007-2013 Carlie J. Coats, Jr., and 
    !! (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  subroutine body starts at line  62
    !!
    !!  FUNCTION:  
    !!       Generate complex netCDF-related messages for I/O API,
    !!       and call NF_ABOR() on the related file
    !!
    !!  PRECONDITIONS REQUIRED:  
    !!       message fits on one line
    !!       netCDF 3.x or later
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:  INIT3, TRIMLEN, NF_STRERROR
    !!
    !!  REVISION  HISTORY:   
    !!      Adapted  9/1999 by CJC from M3MESG()
    !!      Modified 9/1999 by CJC for AIX "flush_"
    !!      Modified 9/1999 by CJC for AIX "flush_"
    !!      Modified 7/2003 by CJC:  factor through M3MSG2(); declare TRIMLEN()
    !!      Bug-fix  2/2005 by CJC:  start with M3MSG2(MESSAGE) not M3MSG2(MESG)
    !!      Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !!      Modified 08/2015 by CJC: USE MODNCFIO.  Call NF_STRERROR().
    !!      F90 "free" source format.
    !!      Modified 08/2015 by CJC: 
    !!***********************************************************************

    USE MODNCFIO

    IMPLICIT NONE

    !!...........   ARGUMENTs and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME           !  file name
    INTEGER      , INTENT(IN   ) :: FNUM            !  netCDF file ID
    INTEGER      , INTENT(IN   ) :: IERR            !  netCDF error number
    CHARACTER*(*), INTENT(IN   ) :: MESSAGE


    !!.......   Scratch LOCAL VARIABLES

    INTEGER         JERR
    CHARACTER*256   MESG       !  fixed-length buffer


    !!***********************************************************************
    !!   begin body of subroutine  M3MSG2

    CALL M3MSG2( MESSAGE )
    WRITE( MESG, '( A, :, I5, 2X, 3A )' )        &
        'netCDF error number', IERR, 'processing file "', TRIM( FNAME ), '"'
    CALL M3MSG2( MESG  )
    MESG = NF_STRERROR( IERR )
    CALL M3MSG2( MESG  )
    JERR = NF_ABORT( FNUM )
    IF ( JERR .NE. 0 ) THEN
        MESG = NF_STRERROR( IERR )
        CALL M3MSG2( MESG  )
    END IF

    RETURN

END SUBROUTINE  M3ABORT

