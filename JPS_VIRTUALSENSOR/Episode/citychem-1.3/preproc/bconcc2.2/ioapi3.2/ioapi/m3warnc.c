
/**************************************************************************
VERSION "$Id: m3warnc.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Generate simple warning messages for Models-3 core

PRECONDITIONS:
    JDATE:JTIME represented as YYYYDDD:HHMMSS

CALLS:
    I/O API's Fortran-binding  M3WARN()

REVISION HISTORY:
    Prototype 4/1995 by CJC

    Version   8/1999 by CJC for I/O APIv2:  FLDMN, Win32

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.

**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN
#define M3WARN m3warn_
#elif defined(__hpux) || defined(_AIX)
#define M3WARN m3warn
#endif


#if defined(M3WARN) || defined(ABSFT)

		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

    extern void M3WARN( const char * caller ,
                        const FINT * jdate ,
                        const FINT * jtime ,
                        const char * errtxt ,
                        FSTR_L       clen ,
                        FSTR_L       elen ) ;

void m3warnc( const char          * caller ,
              int                   jdate ,
              int                   jtime ,
              const char          * errtxt )

    {       /*  begin body of m3warnc() */
    FINT   date, time ;
    date = (FINT) jdate ;
    time = (FINT) jtime ;

    M3WARN( caller, &date, &time, errtxt,
            STRLEN( caller ) , STRLEN( errtxt ) ) ;
    return ;
    }       /*  end body of m3warnc ()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  WIN32 m3warnc(): **/

#elif defined(_WIN32)

    extern void M3WARN( const char * caller ,
                        FSTR_L       clen ,
                        const FINT * jdate ,
                        const FINT * jtime ,
                        const char * errtxt ,
                        FSTR_L       elen ) ;

void m3warnc( const char          * caller ,
              int                   jdate ,
              int                   jtime ,
              const char          * errtxt )

    {       /*  begin body of m3warnc() */
    FINT   date, time ;
    date = (FINT) jdate ;
    time = (FINT) jtime ;

    M3WARN( caller, STRLEN( caller ), &date, &time,
            errtxt, STRLEN( errtxt ) ) ;
    return ;
    }       /*  end body of m3warnc ()  */

                        /** END  CASE OF WIN32 **/
                	/** NEXT CASE:  CRAY CF77-TARGETED m3warnc(): **/

#elif  defined(_CRAY)


#include <fortran.h>

    extern void M3WARN( const _fcd    caller , 
                        const  FINT * jdate  ,
                        const  FINT * jtime  ,
                        const _fcd    errtxt ) ;

void m3warnc( const char          * caller ,
              int                   jdate  ,
              int                   jtime  ,
              const char          * errtxt )

    {       /*  begin body of m3warnc() */
    FINT   date, time ;
    _fcd  cname ;
    _fcd  etext ;

    date = (FINT) jdate ;
    time = (FINT) jtime ;
    cname = _cptofcd( (char *)caller, STRLEN( caller ) ) ;
    etext = _cptofcd( (char *)errtxt, STRLEN( errtxt ) ) ;

    M3WARN( cname, &date, &time, etext ) ;
    return ;

    }       /*  end body of m3warnc ()  */

                	/** END  CASE OF CRAY CF77-TARGETED m3warnc(): **/

#else

#error   "Error compiling m3warnc():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

