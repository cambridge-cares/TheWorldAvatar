
/**************************************************************************
VERSION "$Id: getdfilec.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.
    "getdfilec.c" version "$Id: getdfilec.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Opens Models-3 file with the specified logical name and
    file description.  Wrapper around I/O API Fortran-binding
    routine GETDFILE()

PRECONDITIONS:

CALLS:
    Fortran I/O API utility routine GETDFILE()

REVISION HISTORY:
    Prototype 3/1995 by CJC

    Version   9/1999 by CJC adds WIN32 support

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


                /** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN
#define GETDFILE getdfile_
#elif defined(__hpux) || defined(_AIX)
#define GETDFILE getdfile
#endif


#if defined(GETDFILE) || defined(ABSFT)

                /** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

    extern FINT GETDFILE( const char * fname ,
                          const FINT * rwstatus ,
                          const FINT * fmstatus ,
                          const FINT * reclen   ,
                          const char * pname    ,
                          FSTR_L       fnamelen ,
                          FSTR_L       pnamelen ) ;

int getdfilec( const char * fname  ,
               int          rstatus,
               int          fstatus,
               int          reclen ,
               const char * pname )

    {       /*  begin body of getdfilec() */
    FINT  rwstatus ;
    FINT  fmstatus ;
    
    rwstatus = (FINT)( rstatus != 0 ) ;
    fmstatus = (FINT)( fstatus != 0 ) ;

    return GETDFILE(  fname    ,
                    & rwstatus ,
                    & fmstatus ,
                    & reclen   ,
                      pname    ,
                      STRLEN( fname ) ,
                      STRLEN( pname ) ) ;

    }       /*  end body of getdfilec ()  */

                        /** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                        /** NEXT CASE:  WIN32-TARGETED getdfilec(): **/

#elif  defined(_WIN32)

extern FINT GETDFILE( const char * fname ,
                      FSTR_L       fnamelen ,
                      const FINT * rwstatus ,
                      const FINT * fmstatus ,
                      const FINT * reclen   ,
                      const char * pname    ,
                      FSTR_L       pnamelen ) ;

int getdfilec( const char * fname  ,
               int          rstatus,
               int          fstatus,
               int          reclen ,
               const char * pname )

    {       /*  begin body of getdfilec() */
    FINT  rwstatus ;
    FINT  fmstatus ;
    
    rwstatus = (FINT)( rstatus != 0 ) ;
    fmstatus = (FINT)( fstatus != 0 ) ;

    return GETDFILE(  fname    ,
                      STRLEN( fname ) ,
                    & rwstatus ,
                    & fmstatus ,
                    & reclen   ,
                      pname    ,
                      STRLEN( pname ) ) ;

    }       /*  end body of WIN32 getdfilec ()  */


                        /** END  CASE OF WIN32 F77 TARGETS **/
                        /** NEXT CASE:  CRAY CF77-TARGETED getdfilec(): **/


#elif  defined(_CRAY)


#include <fortran.h>

    extern FINT GETDFILE( const _fcd   fname ,
                          const FINT * rwstatus,
                          const FINT * fmstatus,
                          const FINT * reclen,
                          const _fcd   pname ) ;

int getdfilec( const char          * fname ,
               int                   rstatus,
               int                   fstatus,
               int                   reclen,
               const char          * pname )

    {       /*  begin body of getdfilec() */
    
    _fcd  file ;
    _fcd  prgm ;

    FINT   rstat ;
    FINT   fstat ;

    char  nbuf[  32 ] ;
    char  mbuf[ 256 ] ;

    file = _cptofcd( (char *)fname, STRLEN( fname ) ) ;
    prgm = _cptofcd( (char *)pname, STRLEN( pname ) ) ;

    rstat = _ltob( rstatus ) ;
    fstat = _ltob( fstatus ) ;

    return GETDFILE( file, 
                     &rstat, 
                     &fstat, 
                     &reclen,
                     prgm ) ;
                     
    }       /*  end body of getdfilec ()  */

                        /** END  CASE OF CRAY CF77-TARGETED getdfilec(): **/

#else

#error   "Error compiling getdfilec():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

