/**************************************************************************
VERSION 
    EDSS/Models-3 I/O API.
    check3c.c version  "$Id: check3c.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    returns TRUE iff time step for (JDATE,JTIME) is available
    for variable VNAME in file with logical name FNAME.


PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

CALLS:
    Fortran I/O API's CHECK3()

REVISION HISTORY:
    Prototype 3/1995 by CJC

    Version   8/1999 by CJC:  WIN32 stuff

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN
#define CHECK3 check3_
#elif defined(__hpux) || defined(_AIX)
#define CHECK3 check3
#endif


#if defined(CHECK3) || defined(ABSFT)

		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

    extern FINT CHECK3( const char * fname ,
                        const char * vname ,
                        FINT       * jdate ,
                        FINT       * jtime ,
                        FSTR_L       fnamelen ,
                        FSTR_L       vnamelen ) ;

int check3c( const char *fname ,
             const char *vname ,
             int         jdate ,
             int         jtime )

    {       /*  begin body of check3c() */
    FINT  jd, jt ;

    jd = (FINT) jdate ;
    jt = (FINT) jtime ;
    return CHECK3(   fname , 
                     vname , 
                   & jd , 
                   & jt , 
                     STRLEN( fname ) , 
                     STRLEN( vname ) ) ;

    }       /*  end body of check3c ()  */

#elif defined(_WIN32)

		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

    extern FINT CHECK3( const char * fname ,
                        FSTR_L       fnamelen ,
                        const char * vname ,
                        FSTR_L       vnamelen ,
                        FINT       * jdate ,
                        FINT       * jtime ) ;

int check3c( const char *fname ,
             const char *vname ,
             int         jdate ,
             int         jtime )

    {       /*  begin body of check3c() */

    FINT  jd, jt ;

    jd = (FINT) jdate ;
    jt = (FINT) jtime ;
    return CHECK3(   fname , 
                     STRLEN( fname ) , 
                     vname , 
                     STRLEN( vname ) ,
                   & jd , 
                   & jt ) ;

    }       /*  end body of check3c ()  */

#elif defined(_CRAY)


#include <fortran.h>

    extern FINT CHECK3( const _fcd   fname ,
                       const _fcd   vname ,
                       FINT      * jdate ,
                       FINT      * jtime ) ;

int check3c( const char * fname ,
             const char * vname ,
             int          jdate ,
             int          jtime )
 
    {       /*  begin body of check3c() */
    
    _fcd  file ;
    _fcd  vble ;
    
    FINT  jd, jt ;

    jd = (FINT) jdate ;
    jt = (FINT) jtime ;

    file = _cptofcd( (char *)fname, STRLEN( fname ) ) ;
    vble = _cptofcd( (char *)vname, STRLEN( vname ) ) ;

    return _btol( CHECK3(  file , 
                           vble , 
                         & jd, 
                         & jt ) ) ; 
                     
    }       /*  end body of check3c ()  */

                	/** END  CASE OF CRAY CF77-TARGETED check3c(): **/

#else

#error   "Error compiling check3c():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

