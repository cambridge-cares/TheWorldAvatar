
/**************************************************************************
VERSION "$Id: systemf.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Wrapper around "system()" for Fortran use.

RETURNS:
    return status of "system(<command>)"

PRECONDITIONS:
    Valid Fortran-string command to pass along to "system()"

CALLS:
    system().

REVISION HISTORY:
    Version   8/99 by CJC:  FLDMN, Win32

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "parms3.h"
#include "iodecl3.h"

/** Hack for Feldman-descended f77's follows: **/

#if FLDMN
#define SYSTEMF   systemf_
#elif defined(__hpux) || defined(_AIX)
#define SYSTEMF   systemf
#elif defined(_WIN32) || defined(ABSFT) || defined(_CRAY)
/* DO NOTHING */
#else
#error   "Error compiling SYSTEMF():  unsupported architecture"
#endif

/** -------------------------------------------------------------- **/
/** FIRST CASE:  FELDMANISMS and WIN32:                                    **/
/** -------------------------------------------------------------- **/

#if defined(SYSTEMF) || defined(_WIN32) || defined(ABSFT)

FINT SYSTEMF( char * cmdstr,
              FSTR_L length )
    {
    char cmdbuf[ 4096 ] ;
    fstr2cstr( cmdstr, cmdbuf, (long)length, 4096 ) ;
    return( (FINT) system( cmdbuf ) ) ;
    }

/** -------------------------------------------------------------- **/
/** NEXT CASE:  CRAY CF77-TARGETED ENV*():                         **/
/** -------------------------------------------------------------- **/

#elif  defined(_CRAY)

#include <fortran.h>

FINT SYSTEMF( const _fcd  cmdstr )
    {
    char cmdbuf[ 4096 ] ;
    fstr2cstr( cmdstr, cmdbuf, 4096 ) ;
    return( (FINT) system( cmdbuf ) ) ;
    }

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

