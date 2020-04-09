
/**************************************************************************
VERSION "$Id: read3c.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    reads specified layer of variable "vname" from file with 
    logical name "fname" and puts it into "buffer".
    C wrapper around I/O API Fortran binding routine READ3().


PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

CALLS:
    Fortran I/O API's READ3()

REVISION HISTORY:
    Prototype 3/95 by CJC

    Version   8/99 by CJC:  FLDMN, Win32

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"

#if FLDMN

#define READ3 read3_

#elif defined(__hpux) || defined(_AIX)

#define READ3 read3

#endif


#if defined(READ3) || defined(ABSFT)

		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

    extern FINT READ3( const char * fname ,
                       const char * vname ,
                       FINT       * layer ,
                       FINT       * jdate ,
                       FINT       * jtime ,
                       void       * buffer,
                       FSTR_L       fnamelen ,
                       FSTR_L       vnamelen ) ;

int read3c( const char * fname ,
            const char * vname ,
            int          layer ,
            int          jdate ,
            int          jtime ,
            void       * buffer )

    {       /*  begin body of read3c() */
    FINT   lay, date, time ;

    lay  = (FINT) layer ;
    date = (FINT) jdate ;
    time = (FINT) jtime ;

    return (int) READ3(  fname , 
                         vname , 
                       & lay ,
                       & date , 
                       & time , 
                         buffer,
                         STRLEN( fname ) , 
                         STRLEN( vname ) ) ;

    }       /*  end body of read3c ()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  WIN32 read3c(): **/

#elif defined(_WIN32)

    extern FINT READ3( const char * fname , FSTR_L fnamelen ,
                       const char * vname , FSTR_L vnamelen ,
                       FINT       * layer ,
                       FINT       * jdate ,
                       FINT       * jtime ,
                       void       * buffer );

int read3c( const char * fname ,
            const char * vname ,
            int          layer ,
            int          jdate ,
            int          jtime ,
            void       * buffer )

    {       /*  begin body of read3c() */
    FINT   lay, date, time ;

    lay  = (FINT) layer ;
    date = (FINT) jdate ;
    time = (FINT) jtime ;

    return (int) READ3(  fname , STRLEN( fname ) , 
                         vname , STRLEN( vname ) ,
                       & lay ,
                       & date , 
                       & time , 
                         buffer );

    }       /*  end body of read3c ()  */

                	/** END  CASE OF WIN32 read3c() **/
                	/** NEXT CASE:  CRAY CF77-TARGETED read3c(): **/

#elif  defined(_CRAY)


#include <fortran.h>

    extern FINT READ3( const _fcd    fname ,
                       const _fcd    vname ,
                       const  FINT * layer ,
                       const  FINT * jdate ,
                       const  FINT * jtime ,
                       void        * buffer ) ;

int read3c( const char * fname ,
            const char * vname ,
            int          layer ,
            int          jdate ,
            int          jtime ,
                  void * buffer )
 
    {       /*  begin body of read3c() */
    FINT   lay, date, time ;
    _fcd   file ;
    _fcd   vble ;
    
    lay  = (FINT) layer ;
    date = (FINT) jdate ;
    time = (FINT) jtime ;

    file = _cptofcd( (char *)fname, STRLEN( fname ) ) ;
    vble = _cptofcd( (char *)vname, STRLEN( vname ) ) ;

    return _btol( READ3(  file , 
                          vble , 
                        & lay,
                        & date, 
                        & time,
                          buffer ) ) ; 
                     
    }       /*  end body of read3c ()  */

                	/** END  CASE OF CRAY CF77-TARGETED read3c(): **/

#else

#error   "Error compiling read3c():  unsupported architecture"

#endif              /** #IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

