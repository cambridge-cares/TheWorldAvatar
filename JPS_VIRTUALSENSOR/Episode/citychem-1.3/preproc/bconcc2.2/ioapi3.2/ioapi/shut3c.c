
/**************************************************************************
VERSION "$Id: shut3c.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    shut down I/O API:  C wrapper around Fortran-binding
    routine SHUT3()


PRECONDITIONS:

CALLS:
    Fortran I/O API's SHUT3()

REVISION HISTORY:

    Prototype 3/95 by CJC

    Version    8/99 by CJC:  FLDMN, WIN32 portability enhancements

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "parms3.h"

#if FLDMN

#define SHUT3 shut3_

#elif defined(__hpux) || defined(_AIX)

#define SHUT3 shut3

#endif

		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if defined(SHUT3) || defined(_WIN32) || defined(_CRAY) || defined(ABSFT)

    extern FINT SHUT3( void ) ;

    int shut3c( void ) { 
                       return (int) SHUT3() ; 
                       }

#else

#error   "Error compiling shut3c():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

