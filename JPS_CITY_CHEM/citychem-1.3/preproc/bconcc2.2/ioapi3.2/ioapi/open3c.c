
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "locatsc.c" version "$Id: open3c.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Opens Models-3 file with the specified logical name and
    file description.  Wrapper around I/O API Fortran-binding
    routine OPEN3()

PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

CALLS:
    Fortran I/O API's OPEN3()

REVISION HISTORY:
    Prototype 3/1995 by CJC

    Version   8/1999 by CJCC for I/O APIv2:  FLDMN, Win32

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  <stdio.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN

#define OPEN3 open3_

#elif defined(__hpux) || defined(_AIX)

#define OPEN3 open3

#endif


#if defined(OPEN3) || defined(ABSFT)

    extern FINT OPEN3( const char * fname ,
                       const FINT * status,
                       const char * pname,
                       FSTR_L       fnamelen ,
                       FSTR_L       pnamelen ) ;

int open3c( const char          * fname ,
            const IOAPI_Bdesc3  * bdesc ,
            const IOAPI_Cdesc3  * cdesc ,
            int                   status,
            const char          * pname )

    {       /*  begin body of open3c() */
    char nbuf[  32 ] ;
    char mbuf[ 256 ] ;
    FINT flag ;

    if ( ( status == FSNEW3  ) || 
         ( status == FSUNKN3 ) ||
         ( status == FSCREA3 ) )
        {
        if ( ( ! bdesc ) || ( ! cdesc ) )
            {
            sprintf( nbuf, "%s:%s", pname, "open3c()" ) ;
            sprintf( mbuf, "%s( %s )", 
                           "Null passed to open3c", fname ) ;
            m3errc( nbuf, 0, 0, mbuf, 0 ) ;
            return( 0 ) ;
            }
    
        memcpy( (void*) &BDESC3, (void*) bdesc, sizeof( IOAPI_Bdesc3 ) ) ;
        memcpy( (void*) &CDESC3, (void*) cdesc, sizeof( IOAPI_Cdesc3 ) ) ;
    
        }	/** if opened "new" or "unknown" **/

    flag = (FINT) status ;
    return (int) OPEN3(  fname  ,
                       & flag ,
                         pname  ,
                         STRLEN( fname ) ,
                         STRLEN( pname ) ) ;

    }       /*  end body of open3c ()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  Win32 open3c(): **/

#elif defined(_WIN32)

#include <stdio.h> /* for sprintf */

    extern FINT OPEN3( const char * fname ,
                       FSTR_L       fnamelen ,
                       const FINT * status,
                       const char * pname,
                       FSTR_L       pnamelen ) ;

int open3c( const char          * fname ,
            const IOAPI_Bdesc3  * bdesc ,
            const IOAPI_Cdesc3  * cdesc ,
            int                   status,
            const char          * pname )

    {       /*  begin body of open3c() */
    char nbuf[  32 ] ;
    char mbuf[ 256 ] ;
    FINT flag ;

    if ( ( status == FSNEW3  ) || 
         ( status == FSUNKN3 ) ||
         ( status == FSCREA3 ) )
        {
        if ( ( ! bdesc ) || ( ! cdesc ) )
            {
            sprintf( nbuf, "%s:%s", pname, "open3c()" ) ;
            sprintf( mbuf, "%s( %s )", 
                           "Null passed to open3c", fname ) ;
            m3errc( nbuf, 0, 0, mbuf, 0 ) ;
            return( 0 ) ;
            }
    
        memcpy( (void*) &BDESC3, (void*) bdesc, sizeof( IOAPI_Bdesc3 ) ) ;
        memcpy( (void*) &CDESC3, (void*) cdesc, sizeof( IOAPI_Cdesc3 ) ) ;
    
        }	/** if opened "new" or "unknown" **/

    flag = (FINT) status ) ;
    return (int) OPEN3(  fname  ,
                         STRLEN( fname ) ,
                       & flag ,
                         pname  ,
                         STRLEN( pname ) ) ;

    }       /*  end body of open3c ()  */

                	/** END  CASE OF Win32 **/
                	/** NEXT CASE:  CRAY CF77-TARGETED open3c(): **/

#elif  defined(_CRAY)


#include <fortran.h>

    extern int OPEN3( const _fcd    fname ,
                      const  FINT * status,
                      const _fcd    pname ) ;

    IOAPI_Bdesc3  BDESC3 ;
    IOAPI_Cdesc3  CDESC3 ;


int open3c( const char          * fname ,
            const IOAPI_Bdesc3  * bdesc ,
            const IOAPI_Cdesc3  * cdesc ,
            int                   status,
            const char          * pname )

    {       /*  begin body of open3c() */
    
    _fcd  file ;
    _fcd  prgm ;
    FINT flag ;
    
    char nbuf[  32 ] ;
    char mbuf[ 256 ] ;

    if ( ( status == FSNEW3  ) || 
         ( status == FSUNKN3 ) ||
         ( status == FSCREA3 ) )
        {
        if ( ( ! bdesc ) || ( ! cdesc ) )
            {
            sprintf( nbuf, "%s:%s", pname, "open3c()" ) ;
            sprintf( mbuf, "%s( %s )", 
                           "Null passed to open3c", fname ) ;
            m3errc( nbuf, 0, 0, mbuf, 0 ) ;
            return( 0 ) ;
            }
    
        memcpy( (void*) &BDESC3, (void*) bdesc, sizeof( IOAPI_Bdesc3 ) ) ;
        memcpy( (void*) &CDESC3, (void*) cdesc, sizeof( IOAPI_Cdesc3 ) ) ;
    
        }	/** if opened "new" or "unknown" **/

    file = _cptofcd( (char *)fname, (int) STRLEN( fname ) ) ;
    prgm = _cptofcd( (char *)pname, (int) STRLEN( pname ) ) ;
    flag = (FINT) status ) ;

    return _btol( OPEN3( file, &flag, prgm ) ) ;
                     
    }       /*  end body of open3c ()  */

                	/** END  CASE OF CRAY CF77-TARGETED open3c(): **/

#else

#error   "Error compiling open3c():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

