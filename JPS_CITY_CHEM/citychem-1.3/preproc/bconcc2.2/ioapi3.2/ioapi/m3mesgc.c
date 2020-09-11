
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "m3mesgc.c" version "$Id: m3mesgc.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
	Generate simple error messages for Models-3 core;
	terminate program execution iff FATAL.

PRECONDITIONS:
	JDATE:JTIME represented as YYYYDDD:HHMMSS

CALLS:
	Fortran I/O API utility routine M3MESG()

REVISION HISTORY:
	Prototype 4/1995 by Carlei J Coats, Jr, MCNC Environmental Programs

	Version   8/1999 by CJC -- FLDMN, Win32

	Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
        type resolution modifications
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN
#define M3MESG m3mesg_
#elif defined(__hpux) || defined(_AIX)
#define M3MESG m3mesg
#endif


#if defined(M3MESG) || defined(_WIN32) || defined(ABSFT)


    extern void M3MESG( const char * message ,
                        FSTR_L       mlen ) ;

void m3mesgc( const char * message )
    {       /*  begin body of m3mesgc() */

    M3MESG( message, STRLEN( message ) ) ;
    return ;

    }       /*  end body of m3mesgc ()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  WIN32 m3mesgc(): **/

#elif  defined(_CRAY)


#include <fortran.h>

    extern void M3MESG( const _fcd   message ) ;

void m3mesgc( const char * message )

    {       /*  begin body of m3mesgc() */
    
    _fcd  mtext ;
    
    mtext = _cptofcd( (char *)message, STRLEN( message ) ) ;

    M3MESG( mtext ) ;
    return ;
                     
    }       /*  end body of m3mesgc ()  */

                	/** END  CASE OF CRAY CF77-TARGETED m3mesgc(): **/

#else

#error   "Error compiling m3mesgc():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

