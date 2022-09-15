
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "iobin3.c" version "$Id: m3errc.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Generate simple error messages for Models-3 core;
    terminate program execution iff FATAL.

PRECONDITIONS:
    JDATE:JTIME represented as YYYYDDD:HHMMSS, or 0:0

CALLS:
    I/O API's Fortran-binding routine M3ERR()

REVISION HISTORY:
    Prototype 4/1995 by CJC

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN

#define M3ERR m3err_

#elif defined(__hpux) || defined(_AIX)

#define M3ERR m3err

#endif


#if defined(M3ERR) || defined(ABSFT)

    extern void M3ERR( const char * caller ,
                       const FINT * jdate ,
                       const FINT * jtime ,
                       const char * errtxt ,
                       const FINT * fatal ,
                       FSTR_L       clen ,
                       FSTR_L       elen ) ;

void m3errc( const char          * caller ,
             int                   jdate ,
             int                   jtime ,
             const char          * errtxt ,
             int                   fatal )

    {       /*  begin body of m3errc() */
    FINT   date, time, flag ;
    date = (FINT) jdate ;
    time = (FINT) jtime ;
    flag = (FINT) fatal ;

    M3ERR( caller, &date, &time, errtxt, &flag,
           STRLEN( caller ) , STRLEN( errtxt ) ) ;
    return ;

    }       /*  end body of m3errc ()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  CRAY CF77-TARGETED m3errc(): **/

#elif  defined(_WIN32)


    extern void M3ERR( const char  * caller ,
                       FSTR_L        clen ,
                       const FINT  * jdate ,
                       const FINT  * jtime ,
                       const char  * errtxt ,
                       const FINT  * fatal ,
                       FSTR_L        elen ) ;

void m3errc( const char          * caller ,
             int                   jdate ,
             int                   jtime ,
             const char          * errtxt ,
             int                   fatal )

    {       /*  begin body of m3errc() */
    FINT   date, time, flag ;
    date = (FINT) jdate ;
    time = (FINT) jtime ;
    flag = (FINT) fatal ;

    M3ERR( caller, STRLEN( caller ), &date, &time, 
           errtxt, STRLEN( errtxt ), &flag ) ;
    return ;

    }       /*  end body of m3errc ()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  CRAY CF77-TARGETED m3errc(): **/

#elif  defined(_CRAY)


#include <fortran.h>

    extern void M3ERR( const _fcd    caller , 
                       const  int  * jdate  ,
                       const  FINT * jtime  ,
                       const _fcd    errtxt ,
                       const  FINT * fatal  ) ;

void m3errc( const char          * caller ,
             int                   jdate  ,
             int                   jtime  ,
             const char          * errtxt , 
             int                   fatal )

    {       /*  begin body of m3errc() */
    
    _fcd  cname ;
    _fcd  etext ;
     FINT   date, time, flag ;

   
    cname = _cptofcd( (char *)caller, STRLEN( caller ) ) ;
    etext = _cptofcd( (char *)errtxt, STRLEN( errtxt ) ) ;
     date = (FINT) jdate ;
     time = (FINT) jtime ;
    flag  = (FINT) _ltob( fatal ) ;

    M3ERR( cname, &date, &time, etext, &flag ) ;
    return ;
                     
    }       /*  end body of m3errc ()  */

                	/** END  CASE OF CRAY CF77-TARGETED m3errc(): **/

#else

#error   "Error compiling m3errc():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

