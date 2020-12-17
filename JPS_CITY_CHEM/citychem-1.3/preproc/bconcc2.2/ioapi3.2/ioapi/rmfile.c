
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API.
    "rmfile.c" version "$Id: rmfile.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Fortran-callable "remove file" 
	
PRECONDITIONS:

CALLS:
    I/O API's Fortran binding routine INIT3()

REVISION HISTORY:
    ??

    Version   8/1999 by CJC for I/O APIv2:  WIN32 stuff

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.

**************************************************************************/

/*   (preprocessor DEFINES and INCLUDES here)  */

#if defined(__hpux)		/** -->!!!HACK!!!<---  **/
#define _SYS_UNISTD_INCLUDED
#endif

#include <string.h>
#include <errno.h>
#include "parms3.h"
#include "iodecl3.h"

          
#if                     FLDMN

#define RMFILE rmfile_
#include <unistd.h>
#include <ctype.h>

#elif                   defined(__hpux) || defined(_AIX)

#define RMFILE rmfile
#include <unistd.h>
#include <ctype.h>

#elif                   defined(_WIN32)

#include <stdio.h>
#include <ctype.h>
#define unlink _unlink

#endif


#define  BUFLEN  256

extern void m3warnc( const char   * caller ,
	             int            jdate ,
	             int            jtime ,
	             const char   * errtxt ) ;


#if  defined(RMFILE) || defined(_WIN32) || defined(ABSFT)

/** Hack for Feldman-descended f77's follows: **/


FINT RMFILE( const char *path, FSTR_L length )
    {       /*  begin body of DUMMY() */
    char  buffer[ BUFLEN + 1 ] ;
    int   status ;
    char *mesg ;

    name2cstr( path, buffer, length, BUFLEN + 1 ) ;

    if ( status = unlink( buffer ) )
        {
        if ( mesg = strerror( errno ) )
            {
            m3warnc( "RMFILE", 0, 0, mesg ) ;
            }
        else{
            m3warnc( "RMFILE", 0, 0, "Error unlinking file" ) ;
            } ;
        } ;

    return( status ) ;

    }       /*  end body of RMFILE()  */



#elif  defined(_CRAY)

#include <fortran.h>

FINT RMFILE( const _fcd  path )
    {       /*  begin body of DUMMY() */
    char   buffer[ BUFLEN + 1 ] ;
    int    status ;
    char * mesg ;

    name2cstr( path, buffer, BUFLEN + 1 ) ;

    if ( status = unlink( buffer ) )
        {
        if ( mesg = strerror( errno ) )
            {
            m3warnc( "RMFILE", 0, 0, mesg ) ;
            }
        else{
            m3warnc( "RMFILE", 0, 0, "Error unlinking file" ) ;
            } ;
        } ;

    return( status ) ;

    }       /*  end body of RMFILE()  */


#else
#error   "Error compiling RMFILE():  unsupported architecture"
#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/
    
