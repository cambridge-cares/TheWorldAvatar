/**************************************************************************
VERSION "$Id: close3c.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Close Models-3 file with the specified logical name.  
    Wrapper around I/O API Fortran-binding routine CLOSE3()

PRECONDITIONS:

    **  NOT TO BE USED IN MODELING CODES  **
    **  by order of Joan Novak, EPA ORD, and Ed Bilicki, MCNC EMC

    FNAME already opened by CLOSE3() or close3c()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

CALLS:
    Fortran I/O API's CLOSE3()

REVISION HISTORY:
    Prototype 8/1995 by CJC

    Version   8/1999 by CJC:  WIN32 stuff

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN
#define CLOSE3 close3_
#elif  defined(__hpux) || defined(_AIX)
#define CLOSE3 close3
#endif


#if  defined(CLOSE3) || defined(_WIN32) || defined(ABSFT)

    extern FINT CLOSE3( const char * fname,
                       FSTR_L       namlen ) ;
                     
int close3c( const char          * fname )

    {       /*  begin body of close3c() */
    return CLOSE3(  fname , (int) STRLEN( fname ) ) ;
    }       /*  end body of close3c ()  */

                	/** END  CASE OF Feldman-ish close3c() **/
                	/** NEXT CASE:  CRAY CF77-TARGETED close3c(): **/


#elif  defined(_CRAY)

#include <fortran.h>

    extern FINT CLOSE3( const _fcd   fname ) ;

int close3c( const char          * fname )

    {       /*  begin body of close3c() */
    
    _fcd  file ;

    file = _cptofcd( (char *)fname, (int) STRLEN( fname ) ) ;
    return _btol( CLOSE3( file ) ) ;
                     
    }       /*  end body of close3c ()  */

                	/** END  CASE OF CRAY CF77-TARGETED close3c(): **/

#else

#error   "Error compiling close3c():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

