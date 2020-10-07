/**************************************************************************
VERSION "$Id: desc3c.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    returns file descriptions for open M3 files.
    C wrapper around Fortran-binding routine DESC3().


PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()

CALLS:
    I/O API's Fortran-binding DESC3()

REVISION HISTORY:
    Prototype 3/1995 by CJC

    Version   8/1999 by CJC for I/O APIv2:  WIN32 stuff

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN

#define DESC3 desc3_

#elif defined(__hpux) || defined(_AIX)

#define DESC3 desc3

#endif


#if defined(DESC3) || defined(_WIN32 ) || defined(ABSFT)

    extern FINT DESC3( const char  *fname ,
                       const FSTR_L fnamelen ) ;

int desc3c( const char    * fname ,
            IOAPI_Bdesc3  * bdesc ,
            IOAPI_Cdesc3  * cdesc )

    {       /*  begin body of desc3c() */

    if( ! bdesc ) return( 0 ) ;
    if( ! cdesc ) return( 0 ) ;

    if ( DESC3( fname , STRLEN( fname ) ) )
        {
        memcpy( (void*) bdesc, (void*) &BDESC3, sizeof( IOAPI_Bdesc3 ) ) ;
        memcpy( (void*) cdesc, (void*) &CDESC3, sizeof( IOAPI_Cdesc3 ) ) ;
        return 1 ;        }
    else{
        return 0 ;
        }

    }       /*  end body of desc3c ()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  CRAY CF77-TARGETED desc3c(): **/


#elif  defined(_CRAY)


#include <fortran.h>

    extern FINT DESC3( const _fcd   fname ) ;

int desc3c( const char    * fname ,
            IOAPI_Bdesc3  * bdesc ,
            IOAPI_Cdesc3  * cdesc )

    {       /*  begin body of desc3c() */
    
    _fcd  file ;
    
    if( ! bdesc ) return( 0 ) ;
    if( ! cdesc ) return( 0 ) ;

    file = _cptofcd( (char *)fname, STRLEN( fname ) ) ;

    if ( _btol( DESC3( file ) ) )
        {
        memcpy( (void*) bdesc, (void*) &BDESC3, sizeof( IOAPI_Bdesc3 ) ) ;
        memcpy( (void*) cdesc, (void*) &CDESC3, sizeof( IOAPI_Cdesc3 ) ) ;
        return 1 ;
        }
    else{
        return 0 ;
        }
                     
    }       /*  end body of desc3c ()  */

                	/** END  CASE OF CRAY CF77-TARGETED desc3c(): **/

#else

#error   "Error compiling desc3c():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

