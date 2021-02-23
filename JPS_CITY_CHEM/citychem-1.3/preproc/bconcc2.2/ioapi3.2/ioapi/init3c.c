/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API.
    "init3c.c" version "$Id: init3c.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    start up Models-3 I/O API
    (C wrapper around I/O API Fortran-binding routine INIT3()

PRECONDITIONS:

CALLS:
    I/O API's Fortran binding routine INIT3()

REVISION HISTORY:
    Prototype 3/95 by CJC

    Version   8/99 by CJC for I/O APIv2:  WIN32 stuff

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.

**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN
#define INIT3 init3_
#elif defined(__hpux) || defined(_AIX) || defined(ABSFT)
#define INIT3 init3
#endif


#if defined(INIT3) || defined(_WIN32) || defined(_CRAY)

    extern FINT INIT3( void ) ;

int init3c( void )
    {
    return (int) INIT3() ;
    }


#else

#error   "Error compiling init3c():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

