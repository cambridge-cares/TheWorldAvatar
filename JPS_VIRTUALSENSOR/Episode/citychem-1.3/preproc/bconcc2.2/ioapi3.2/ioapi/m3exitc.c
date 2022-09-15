
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "m3exitc.c" version "$Id: m3exitc.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Generate simple error messages for Models-3 core;
    shut down I/O API via SHUT3() and terminate program 
   execution via exit( errstat )

PRECONDITIONS:
    JDATE:JTIME represented as YYYYDDD:HHMMSS

CALLS:
    I/O API's Fortran-binding  M3EXIT()

REVISION HISTORY:
    Prototype 4/1995 by CJC

    Version   8/1999 by CJC for I/O APIv2:  Win32, OpenMP

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"

		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN
#define M3EXIT m3exit_
#elif defined(__hpux) || defined(_AIX)
#define M3EXIT m3exit
#endif

#if defined(M3EXIT) || defined(ABSFT)

    extern void M3EXIT( const char * caller ,
                        const FINT * jdate ,
                        const FINT * jtime ,
                        const char * errtxt ,
                        const FINT * errstat ,
                        FSTR_L       clen ,
                        FSTR_L       elen ) ;

void m3exitc( const char * caller ,
              int          jdate ,
              int          jtime ,
              const char * errtxt ,
              int          errstat )

    {       /*  begin body of m3exitc() */
    FINT   date, time, flag ;
    date = (FINT) jdate ;
    time = (FINT) jtime ;
    flag = (FINT) errstat ;

    M3EXIT( caller, &date, &time, errtxt, &flag,
           STRLEN( caller ) , STRLEN( errtxt ) ) ;
    return ;

    }       /*  end body of m3exitc ()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  WIN32 m3exitc(): **/

#elif defined(_WIN32)

    extern void M3EXIT( const char * caller ,
                        FSTR_L       clen ,
                        const FINT * jdate ,
                        const FINT * jtime ,
                        const char * errtxt ,
                        FSTR_L       elen ,
                        const FINT * errstat );

void m3exitc( const char * caller ,
              int          jdate ,
              int          jtime ,
              const char * errtxt ,
              int          errstat )

    {       /*  begin body of m3exitc() */
    FINT   date, time, flag ;
    date = (FINT) jdate ;
    time = (FINT) jtime ;
    flag = (FINT) errstat ;

    M3EXIT( caller, STRLEN( caller ), &date, &time,
            errtxt, STRLEN( errtxt ), &flag );

    return ;

    }       /*  end body of m3exitc ()  */

                	/** END  CASE OF WIN32 **/
                	/** NEXT CASE:  CRAY CF77-TARGETED m3exitc(): **/

#elif  defined(_CRAY)


#include <fortran.h>

    extern void M3EXIT( const _fcd    caller , 
                        const  FINT * jdate  ,
                        const  FINT * jtime  ,
                        const _fcd    errtxt ,
                        const  FINT * errstat ) ;

void m3exitc( const char * caller ,
              int          jdate  ,
              int          jtime  ,
              const char * errtxt , 
              int          errstat )

    {       /*  begin body of m3exitc() */
    FINT  date, time, flag ;
    _fcd  cname ;
    _fcd  etext ;

    date = (FINT) jdate ;
    time = (FINT) jtime ;
    flag = (FINT) errstat ;
    cname = _cptofcd( (char *)caller, STRLEN( caller ) ) ;
    etext = _cptofcd( (char *)errtxt, STRLEN( errtxt ) ) ;

    M3EXIT( cname, &date, &time, etext, &flag ) ;
    return ;

    }       /*  end body of m3exitc ()  */

                	/** END  CASE OF CRAY CF77-TARGETED m3exitc(): **/

#else

#error   "Error compiling m3exitc():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

