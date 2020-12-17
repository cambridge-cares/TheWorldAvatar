
/**************************************************************************
VERSION "$Id: write3c.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Writes specified variable "vname" to file with 
    logical name "fname" from "buffer".
    C wrapper around I/O API Fortran binding routine WRITE3().

PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

CALLS:
    Fortran I/O API's WRITE3()

REVISION HISTORY:
    Prototype 3/1995 by CJC

    Version   8/1999 by CJC:  FLDMN, WIN32 portability enhancements

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"

		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/
#if FLDMN       /*  see parms3.h  */

#define WRITE3 write3_

#elif defined(__hpux) || defined(_AIX)

#define WRITE3 write3

#endif


#if defined(WRITE3) || defined(ABSFT)

    extern FINT WRITE3( const char *fname ,
                        const char *vname ,
                        FINT       *jdate ,
                        FINT       *jtime ,
                        const void *buffer,
                        FSTR_L      fnamelen ,
                        FSTR_L      vnamelen ) ;

int write3c( const char * fname ,
             const char * vname ,
             int          jdate ,
             int          jtime ,
             const void * buffer )

    {       /*  begin body of write3c() */
    FINT  date, time ;

    date = (FINT) jdate ;
    time = (FINT) jtime ;

    return (int) WRITE3(  fname , 
                          vname , 
                        & date , 
                        & time , 
                          buffer,
                          STRLEN( fname ) , 
                          STRLEN( vname ) ) ;

    }       /*  end body of write3c ()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  WIN32 write3c(): **/

#elif  defined(_WIN32)

    extern FINT WRITE3( const char *fname    , FSTR_L  fnamelen ,
                        const char *vname    , FSTR_L  vnamelen ,
                        FINT       *jdate    ,
                        FINT       *jtime    ,
                        const void *buffer ) ;

int write3c( const char * fname ,
             const char * vname ,
             int          jdate ,
             int          jtime ,
             const void * buffer )

    {       /*  begin body of write3c() */
    FINT  date, time ;

    date = (FINT) jdate ;
    time = (FINT) jtime ;

    return (int) WRITE3(  fname , STRLEN( fname ) , 
                          vname , STRLEN( vname ) , 
                        & date  , 
                        & time  , 
                          buffer ) ;

    }       /*  end body of write3c ()  */

                	/** END  CASE OF WIN32 write3c() **/
                	/** NEXT CASE:  CRAY CF77-TARGETED write3c(): **/

#elif  defined(_CRAY)


#include <fortran.h>

    extern FINT WRITE3( const _fcd    fname ,
                        const _fcd    vname ,
                        const  FINT * jdate ,
                        const  FINT * jtime ,
                        const  void * buffer ) ;

int write3c( const char  * fname ,
             const char  * vname ,
             int           jdate ,
             int           jtime ,
             const void  * buffer )
 
    {       /*  begin body of write3c() */
    FINT  date, time ;
    _fcd  file ;
    _fcd  vble ;

    date = (FINT) jdate ;
    time = (FINT) jtime ;

    file = _cptofcd( (char *)fname, STRLEN( fname ) ) ;
    vble = _cptofcd( (char *)vname, STRLEN( vname ) ) ;

    return _btol( WRITE3(  file , 
                           vble , 
                         & date, 
                         & time,
                           buffer ) ) ; 
                     
    }       /*  end body of write3c ()  */

                	/** END  CASE OF CRAY CF77-TARGETED write3c(): **/

#else

#error   "Error compiling write3c():  unsupported architecture"

#endif              /** #IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

