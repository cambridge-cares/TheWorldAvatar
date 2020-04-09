/**************************************************************************
VERSION "$Id: ddtvar3c.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Computes time derivative (per second) of specified variable 
    "vname" from file with logical name "fname" from "buffer".
    C wrapper around I/O API Fortran binding routine DDTVAR3().


PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

CALLS:
    Fortran I/O API's DDTVAR3()

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
#define DDTVAR3 ddtvar3_
#elif defined(__hpux) || defined(_AIX)
#define DDTVAR3 ddtvar3
#endif


#if defined(DDTVAR3) || defined(ABSFT)

                /** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

    extern FINT DDTVAR3( const char  * fname ,
                         const char  * vname ,
                         const char  * cname ,
                         FINT        * jdate ,
                         FINT        * jtime ,
                         FINT        * bsize ,
                         FREAL       * buffer,
                         FSTR_L        fnamelen ,
                         FSTR_L        vnamelen ,
                         FSTR_L        cnamelen ) ;

int ddtvar3c( const char  * fname ,
              const char  * vname ,
              const char  * cname ,
              int           jdate ,
              int           jtime ,
              int           bsize ,
              FREAL       * buffer )

    {       /*  begin body of ddtvar3c() */
    FINT  jd, jt, bs ;
    jd = (FINT) jdate ;
    jt = (FINT) jtime ;
    bs = (FINT) bsize ;

    return DDTVAR3(  fname , 
                     vname , 
                     cname , 
                   & jd , 
                   & jt , 
                   & bs , 
                     buffer,
                     STRLEN( fname ) , 
                     STRLEN( vname ) , 
                     STRLEN( cname ) ) ;

    }       /*  end body of ddtvar3c()  */

                        /** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                        /** NEXT CASE:  WIN32 ddtvar3c(): **/

#elif defined(_WIN32)

    extern int DDTVAR3( const char  * fname , FSTR_L  fnamelen ,
                        const char  * vname , FSTR_L  vnamelen ,
                        const char  * cname , FSTR_L  cnamelen ,
                        FINT        * jdate ,
                        FINT        * jtime ,
                        FINT        * bsize ,
                        FREAL       * buffer );

int ddtvar3c( const char  * fname ,
              const char  * vname ,
              const char  * cname ,
              int           jdate ,
              int           jtime ,
              int           bsize ,
              FREAL       * buffer )

    {       /*  begin body of ddtvar3c() */
    FINT  jd, jt, bs ;
    jd = (FINT) *jdate ;
    jt = (FINT) *jtime ;
    bs = (FINT) *bsize ;

    return DDTVAR3(  fname , STRLEN( fname ) , 
                     vname , STRLEN( vname ) ,
                     cname , STRLEN( cname ) ,
                   & jd , 
                   & jt , 
                   & bs , 
                     buffer );

    }       /*  end body of ddtvar3c()  */

                        /** END  CASE OF WIN32 F77 TARGETS **/
                        /** NEXT CASE:  CRAY CF77-TARGETED ddtvar3c(): **/

#elif  defined(_CRAY)


#include <fortran.h>

    extern int DDTVAR3( const _fcd     fname ,
                        const _fcd     vname ,
                        const _fcd     cname ,
                        const  FINT  * jdate ,
                        const  FINT  * jtime ,
                        const  FINT  * bsize ,
                        FREAL        * buffer ) ;

int ddtvar3c( const char  * fname ,
              const char  * vname ,
              const char  * cname ,
              int           jdate ,
              int           jtime ,
              int           bsize ,
              FREAL       * buffer )
 
    {       /*  begin body of ddtvar3c() */
    FINT  jd, jt, bs ;
    _fcd  file ;
    _fcd  vble ;
    _fcd  caller ;
    
    file   = _cptofcd( (char *)fname, STRLEN( fname ) ) ;
    vble   = _cptofcd( (char *)vname, STRLEN( vname ) ) ;
    caller = _cptofcd( (char *)cname, STRLEN( cname ) ) ;

    jd = (FINT) *jdate ;
    jt = (FINT) *jtime ;
    bs = (FINT) *bsize ;

    return _btol( DDTVAR3(  file  , 
                            vble  , 
                            caller  , 
                          & jd , 
                          & jt ,
                          & bs ,
                            buffer ) ) ; 
                     
    }       /*  end body of ddtvar3c ()  */

                        /** END  CASE OF CRAY CF77-TARGETED ddtvar3c(): **/

#else

#error   "Error compiling ddtvar3c():  unsupported architecture"

#endif              /** #IF FELDMAN-DESCENDED F77 TARGETED, OR #IF CRAY **/

