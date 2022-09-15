/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "iobin3.c" version "$Id: get_endian.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 2004-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
        Return integer token for byte order of the caller:
        LITTLE_ENDIAN	1234, e.g., x86
        BIG_ENDIAN	4321, e.g., SGI or Sun
        PDP_ENDIAN	3412, LSB first in word, MSW first in long

CALLS:  none

REVISION HISTORY:
    Prototype 5/2004 by Carlie J. Coats, Jr., BAMS

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.

-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

#include "iodecl3.h"

typedef char    INT4 [ 4 ] ;     /** header representation for "int32" **/

int    get_endian()
    {
    long        jj, kk;
    int         b0, b1, b2, b3 ;
    union       UU { long ii ; INT4 i4 ; } uu;

    for( uu.ii = 0 , jj = 1, kk = 1; jj < 5 ; jj++, kk*=256 )
        {
        uu.ii += jj*kk ;
        }

    b0 = (int)uu.i4[0] ;
    b1 = (int)uu.i4[1] ;
    b2 = (int)uu.i4[2] ;
    b3 = (int)uu.i4[3] ;
    return( b3 + 10*( b2 + 10*( b1 + 10*b0 ) )  ) ;
    }

#ifdef SECOND_UNDERSCORE

#   define    GET_ENDIAN      get_endian__

    FINT  GET_ENDIAN()
        {
        return ( (FINT) get_endian() ) ;
        } ;

#elif FLDMN || defined(_WIN32)

#   define    GET_ENDIAN      get_endian_

    FINT  GET_ENDIAN()
        {
        return ( (FINT) get_endian() ) ;
        } ;

#elif defined(__hpux) || defined(_AIX) || defined(_CRAY)
    /*  do nothing:  C call works directly  */

#elif defined(ABSFT)

    FINT  GET_ENDIAN()
        {
        return ( (FINT) get_endian() ) ;
        } ;

#else
#    error   "Error compiling:  unsupported architecture"
#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

