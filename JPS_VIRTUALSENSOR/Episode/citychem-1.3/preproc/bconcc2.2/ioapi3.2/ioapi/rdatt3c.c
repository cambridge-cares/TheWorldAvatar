
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "locatsc.c" version "$Id: rdatt3c.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

 PURPOSE:
    Reads the attribute named ANAME for the variable VNAME in the
    file FNAME into AVAL[].  If VNAME == ALLVAR3, reads global
    attribute ANAME.
    Wrapper around I/O API Fortran-binding routines RDATT3()
    for numeric attributes, and RDATTC() for character string
    attributes)

PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

CALLS:
    Fortran I/O API's RDATT3(), RDATTC()

REVISION HISTORY:
    Prototype 1/2002 by CJC, MCNC

    Modified  2/2003 by CJC, BAMS:  bug-fix for Cray case; 
    bug discovered by David Wong, US EPA.

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications, native-binary (BINFIL3) files.

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED FC'S FOLLOW: **/

#if FLDMN

#define RDATT3 rdatt3_
#define RDATTC rdattc_

#elif defined(__hpux) || defined(_AIX)

#define RDATT3 rdatt3
#define RDATTC rdattc

#endif


#if defined(RDATT3) || defined(ABSFT)

    extern FINT RDATT3( const char * fname ,
                        const char * vname ,
                        const char * aname ,
                        const FINT * atyp,
                        const FINT * amax,
                        FINT       * asize,
                        void       * aval ,
                        FSTR_L       fnamelen, 
                        FSTR_L       vnamelen, 
                        FSTR_L       anamelen ) ;

    extern FINT RDATTC( const char * fname ,
                        const char * vname ,
                        const char * aname ,
                        char       * aval ,
                        FSTR_L       fnamelen, 
                        FSTR_L       vnamelen, 
                        FSTR_L       anamelen, 
                        FSTR_L       avallen ) ;

int rdatt3c( const char          * fname ,
             const char          * vname ,
             const char          * aname ,
             int                   atyp ,
             int                   amax ,
             int                 * asiz ,
             void                * att )

    {       /*  begin body of rdatt3c() */
    FSTR_L  flen, vlen,  alen, rlen ;
    FINT    asize, amaxx, atype ;
    int     result ;

    flen  = STRLEN( fname ) ;
    vlen  = STRLEN( vname ) ;
    alen  = STRLEN( aname ) ;

    if ( atyp == M3CHAR )
        {
        rlen  = STRLEN( aname ) ;
        return( (int) RDATTC( fname ,
                              vname ,
                              aname ,
                              att   ,
                              flen  ,
                              vlen  ,
                              alen  ,
                              rlen  ) ) ;
        }
    else
        {
        amaxx = amax ;
        atype = atyp ;
        result = (int) RDATT3( fname ,
                               vname ,
                               aname ,
                             & atype ,
                             & amaxx ,
                             & asize ,
                               att   ,
                               flen  ,
                               vlen  ,
                               alen  ) ;
        * asiz = asize ;
        return result ;
        }

    }       /*  end body of  rdatt3c()  */


                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  Win32 rdatt3c(): **/

#elif defined(_WIN32)

#include <stdio.h> /* for sprintf */

    extern FINT RDATT3( const char * fname , FSTR_L fnamelen ,
                        const char * vname , FSTR_L vnamelen ,
                        const char * aname , FSTR_L anamelen ,
                        const FINT * atype,
                        const FINT * amax ,
                        FINT       * asize,
                        void       * aval ) ;

    extern FINT RDATTC( const char * fname , FSTR_L fnamelen ,
                        const char * vname , FSTR_L vnamelen ,
                        const char * aname , FSTR_L anamelen ,
                        char       * aval  , FSTR_L avallen ) ;


int rdatt3c( const char          * fname ,
             const char          * vname ,
             const char          * aname ,
             int                   atyp ,
             int                   amax ,
             int                 * asiz ,
             void                * att )

    {       /*  begin body of  rdatt3c() */
    FSTR_L  flen, vlen,  alen, rlen ;
    FINT    asize, amaxx, atype ;
    int     result ;

    flen  = STRLEN( fname ) ;
    vlen  = STRLEN( vname ) ;
    alen  = STRLEN( aname ) ;

    if ( atyp == M3CHAR )
        {
        rlen  = STRLEN( aname ) ;
        return( (int) RDATTC( fname ,
                              vname ,
                              aname ,
                              att   ,
                              flen ,
                              vlen ,
                              alen ,
                              rlen ) ) ;
        }
    else
        {
        amaxx = amax ;
        atype = atyp ;
        result = (int) RDATT3( fname ,
                               vname ,
                               aname ,
                             & atype ,
                             & amaxx ,
                             & asize ,
                               att   ,
                               flen ,
                               vlen ,
                               alen ) ) ;
        * asiz = asize ;
        return result ;
        }

    }       /*  end body of _WIN32 rdatt3c()  */


                	/** END  CASE OF Win32 **/
                	/** NEXT CASE:  CRAY-TARGETED rdatt3c(): **/

#elif  defined(_CRAY)   /** treatment:  CRAY arryas of strings?? **/


#include <fortran.h>

    extern FINT RDATT3( const _fcd   fname ,
                        const _fcd   vname ,
                        const _fcd   aname ,
                        const FINT * atype,
                        const FINT * amax,
                        FINT       * asize,
                        void       * aval ) ;

    extern FINT RDATTC( const _fcd   fname ,
                        const _fcd   vname ,
                        const _fcd   aname ,
                        _fcd         aval ) ;

int rdatt3c( const char          * fname ,
             const char          * vname ,
             const char          * aname ,
             int                   atyp ,
             int                   amax ,
             int                 * asiz ,
             void                * att )

    {       /*  begin body of  rdatt3c() */
    FINT    asize, amaxx, atype ;
    int     flen, vlen,  alen, rval ;

    _fcd  file ;
    _fcd  vble ;
    _fcd  atnm ;
    _fcd  aval ;

    flen  = STRLEN( fname ) ;
    vlen  = STRLEN( vname ) ;
    alen  = STRLEN( aname ) ;

    file = _cptofcd( (char *)fname, flen ) ;
    vble = _cptofcd( (char *)vname, vlen ) ;
    atnm = _cptofcd( (char *)aname, alen ) ;

    if ( atyp != M3CHAR )
        {
        amaxx = amax ;
        atype = atyp ;
        rval  =  _btol( RDATT3( file ,
                                vble ,
                                atnm ,
                              & atype ,
                              & amaxx ,
                              & asize ,
                                att ) ) ) ;
        * asiz = asize ;
        }
    else{
        aval = _cptofcd( (char *)att, amax ) ;
        rval =  _btol( RDATTC( file ,
                               vble ,
                               atnm ,
                               aval ) ) ;
        if ( rval ) 
            {
            att = (void *) _fcdtocp( aval ) ;
            } ;
        } ;

    return( rval ) ;

    }       /*  end body of  rdatt3c()  */

#else

#error   "Error compiling rdatt3c():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED "FC" TARGETED, 
                        OR IF Win32, OR IF CRAY **/



