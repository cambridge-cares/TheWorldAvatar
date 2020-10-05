/**************************************************************************
VERSION "$Id: filchk3c.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Checks to see whether file FNAME has the indicated type FTYPE,
    and appropriate dimensions NCOLS, NROWS, NLAYS, NTHIK 
    (with checking of just those that are appropriate for each FTYPE).
    Layer-checking may be suppressed by setting NLAYS=ALLAYS3
    C wrapper around Fortran-binding routine FILCHK3().
    Returns TRUE iff the file is already open, and has the user-supplied
    indicated file type and grid/array dimensions.

PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()

CALLS:
    I/O API's Fortran-binding routine FILCHK3()

REVISION HISTORY:
    Prototype 10/2000 by CJC

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN

#define FILCHK3 filchk3_

#elif defined(__hpux) || defined(_AIX)

#define FILCHK3 filchk3

#endif


#if defined(FILCHK3) || defined(ABSFT)

    extern FINT FILCHK3( const char  *fname ,
                         const FINT  *ftype ,
                         const FINT  *ncols ,
                         const FINT  *nrows ,
                         const FINT  *nlays ,
                         const FINT  *nthik ,
                         const FSTR_L fnamlen ) ;

int filchk3c( const char * fname ,
              const int    ftype ,
              const int    ncols ,
              const int    nrows ,
              const int    nlays ,
              const int    nthik )

    {
    FINT  type, cols, rows, lays, thik ;

    type = (FINT) ftype ;
    cols = (FINT) ncols ;
    rows = (FINT) nrows ;
    lays = (FINT) nlays ;
    thik = (FINT) nthik ;

    return( (int) FILCHK3( fname , 
                          &type , &cols , &rows , &lays , &thik ,
                          STRLEN( fname ) ) ) ;
    }

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  WIN32-TARGETED filchk3c(): **/


#elif  defined(_WIN32)

    extern FINT FILCHK3( const char  *fname ,
                         const FSTR_L fnamlen ,
                         const FINT  *ftype ,
                         const FINT  *ncols ,
                         const FINT  *nrows ,
                         const FINT  *nlays ,
                         const FINT  *nthik ) ;

int filchk3c( const char * fname ,
              const int    ftype ,
              const int    ncols ,
              const int    nrows ,
              const int    nlays ,
              const int    nthik )

    {
    FINT  type, cols, rows, lays, thik ;

    type = (FINT) * ftype ;
    cols = (FINT) * ncols ;
    rows = (FINT) * nrows ;
    lays = (FINT) * nlays ;
    thik = (FINT) * nthik ;

    return( (int) FILCHK3( fname , STRLEN( fname ) ,
                          &type , &cols , &rows , &lays , &thik ) ) ;
    }

                	/** END  CASE OF WIN32 TARGETS **/
                	/** NEXT CASE:  CRAY CF77-TARGETED filchk3c(): **/


#elif  defined(_CRAY)


#include <fortran.h>

    extern int FILCHK3( const _fcd    fname,
                        const FINT   *ftype ,
                        const FINT   *ncols ,
                        const FINT   *nrows ,
                        const FINT   *nlays ,
                        const FINT   *nthik ) ;

int filchk3c( const char * fname ,
              const int    ftype ,
              const int    ncols ,
              const int    nrows ,
              const int    nlays ,
              const int    nthik )

    {
    FINT  type, cols, rows, lays, thik ;
    _fcd  file ;
    
    type = (FINT) * ftype ;
    cols = (FINT) * ncols ;
    rows = (FINT) * nrows ;
    lays = (FINT) * nlays ;
    thik = (FINT) * nthik ;

    file = _cptofcd( (char *)fname, STRLEN( fname ) ) ;

    return ( _btol( FILCHK3( file , 
                    &type , &cols , &rows , &lays , &thik ) ) ) ;
    }

                	/** END  CASE OF CRAY CF77-TARGETED filchk3c(): **/

#else

#error   "Error compiling filchk3c():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

