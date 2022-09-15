
/**************************************************************************
VERSION "$Id: sync3c.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

 PURPOSE:
    Performs disk-synchronization for netCDF-based file FNAME.
    Wrapper around I/O API Fortran-binding routines SYNC3().

 PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()

 CALLS:
    Fortran I/O API's SYNC3()

 REVISION HISTORY:
    Prototype 1/2002 by C

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED FC'S FOLLOW: **/

#if FLDMN

#define SYNC3 sync3_

#elif defined(__hpux) || defined(_AIX)

#define SYNC3 sync3

#endif


#if defined(SYNC3) || defined(ABSFT)

    extern FINT SYNC3( const char * fname , FSTR_L fnamelen ) ;


int sync3c( const char * fname )

    {
    return( (int) SYNC3( fname , STRLEN( fname ) ) ) ;
    }


                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  Win32 sync3c(): **/

#elif defined(_WIN32)

#include <stdio.h> /* for sprintf */

    extern int SYNC3( const char * fname , FSTR_L fnamelen ) ;


int sync3c( const char * fname )

    {
    return( (int) SYNC3( fname , STRLEN( fname ) ) ) ;
    }


                	/** END  CASE OF Win32 **/
                	/** NEXT CASE:  CRAY-TARGETED sync3c(): **/

#elif  defined(_CRAY)   /** treatment:  CRAY arryas of strings?? **/


#include <fortran.h>

    extern FINT SYNC3( const _fcd   fname  ) ;


int sync3c( const char * fname  )

    {
    _fcd  file ;

    file = _cptofcd( (char *)fname, (int) STRLEN( fname ) ) ;

    return( _btol( SYNC3( file ) ) ) ;
    } 

#else

#error   "Error compiling sync3c():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED "FC" TARGETED, 
                        OR IF Win32, OR IF CRAY **/



