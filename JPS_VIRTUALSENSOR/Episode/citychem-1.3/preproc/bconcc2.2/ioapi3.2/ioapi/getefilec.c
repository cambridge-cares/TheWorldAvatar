
/**************************************************************************
VERSION "$Id: getefilec.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Opens Models-3 file with the specified logical name and
    file description.  Wrapper around I/O API Fortran-binding
    routine GETEFILE()

PRECONDITIONS:
    FNAME already opened by GETEFILE() or getefilec()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

CALLS:
    Fortran I/O API's GETEFILE()

REVISION HISTORY:
    Prototype 3/1995 by CJC

    Version    8/99 by CJC:  FLDMN, WIN32 portability enhancements

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN
#define GETEFILE getefile_
#elif defined(__hpux) || defined(_AIX)
#define GETEFILE getefile
#endif


#if defined(GETEFILE) || defined(ABSFT)

		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

    extern FINT GETEFILE( const char * fname ,
                          const FINT * rwstatus,
                          const FINT * fmstatus,
                          const char * pname,
                          FSTR_L       fnamelen ,
                          FSTR_L       pnamelen ) ;

int getefilec( const char * fname ,
               int          rstatus,
               int          fstatus,
               const char * pname )

    {       /*  begin body of getefilec() */
    FINT  rwstatus ;
    FINT  fmstatus ;
    
    rwstatus = (FINT)( rstatus != 0 ) ;
    fmstatus = (FINT)( fstatus != 0 ) ;

    return GETEFILE(  fname  ,
                    & rwstatus ,
                    & fmstatus ,
                      pname  ,
                      STRLEN( fname ) ,
                      STRLEN( pname ) ) ;

    }       /*  end body of getefilec ()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  WIN32-TARGETED getefilec(): **/

#elif defined(_WIN32)

		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

    extern FINT GETEFILE( const char * fname , FINT fnamelen ,
                          const FINT * rwstatus,
                          const FINT * fmstatus,
                          const char * pname, FINT pnamelen ) ;

int getefilec( const char * fname ,
               int          rstatus,
               int          fstatus,
               const char * pname )

    {       /*  begin body of getefilec() */
    char nbuf[  32 ] ;
    char mbuf[ 256 ] ;
    FINT  rwstatus ;
    FINT  fmstatus ;
    
    rwstatus = (FINT)( rstatus != 0 ) ;
    fmstatus = (FINT)( fstatus != 0 ) ;

    return GETEFILE(  fname  , STRLEN( fname ) ,
                    & rwstatus ,
                    & fmstatus ,
                      pname  , STRLEN( pname ) ) ;

    }       /*  end body of getefilec ()  */

                	/** END  CASE OF WIN32 F77 TARGETS **/
                	/** NEXT CASE:  CRAY CF77-TARGETED getefilec(): **/

#elif  defined(_CRAY)


#include <fortran.h>

    extern FINT GETEFILE( const _fcd   fname ,
                          const  FINT * rwstatus,
                          const  FINT * fmstatus,
                          const _fcd   pname ) ;

int getefilec( const char          * fname ,
               int                   rstatus,
               int                   fstatus,
               const char          * pname )

    {       /*  begin body of getefilec() */
    
    _fcd  file ;
    _fcd  prgm ;
    
    FINT   rstat ;
    FINT   fstat ;

    char  nbuf[  32 ] ;
    char  mbuf[ 256 ] ;

    file = _cptofcd( (char *)fname, STRLEN( fname ) ) ;
    prgm = _cptofcd( (char *)pname, STRLEN( pname ) ) ;

    return GETEFILE( file, 
                     &rstat, 
                     &fstat, 
                     prgm ) ;
                     
    }       /*  end body of getefilec ()  */

                	/** END  CASE OF CRAY CF77-TARGETED getefilec(): **/

#else

#error   "Error compiling getefilec():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

