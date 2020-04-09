/**********************************************************************

VERSION "$Id: sleep3.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

    ---->  MACHINE-DEPENDENT !! <----

PURPOSE:
    SUBROUTINE  SLEEP3( INTEGER  SECS )    
    Fortran wrapper around the "sleep()" UNIX system call.
    
REVISION HISTORY:
    Prototype 7/2003 by CJC

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications
**********************************************************************/

#include <unistd.h>
#include "iodecl3.h"

/** Hacks for Feldman-descended Fortrans, and for no-underscore
    quasi-Feldman-descended Fortrans (HP, IBM) follow:
    (NOTE that Cray and Windows Fortrans upcase Fortran symbols,
    so that "SLEEP3()" gives the correct linker-symbol.)        **/

#if FLDMN       

#define SLEEP3   sleep3_

#elif defined(__hpux) || defined(_AIX)

#define SLEEP3   sleep3

#endif

FINT SLEEP3( const FINT * secs )
    {
    unsigned int  isecs ;
    isecs = (int) *secs ;
    if ( isecs > 0 )
        {
        return( (FINT) sleep( isecs ) ) ;
        }
    else{
        return( (FINT) -1 ) ;
        }
    
    }   /*  end SLEEP3()  */
