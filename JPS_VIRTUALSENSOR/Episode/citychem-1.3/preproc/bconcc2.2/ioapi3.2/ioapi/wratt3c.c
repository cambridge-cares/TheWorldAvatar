
/**************************************************************************
VERSION "$Id: wratt3c.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

 PURPOSE:
    Writes the attribute named ANAME for the variable VNAME to the
    file FNAME.  If VNAME == ALLVAR3, writes global attribute ANAME.
    Wrapper around I/O API Fortran-binding routines WRATT3()
    for numeric attributes, and WRATTC() for character string
    attributes)

 PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

 CALLS:
     I/O API's Fortran WRATT3(), WRATTC()

 REVISION HISTORY:
    Prototype 1/2002 by C

    Modified  2/2003 by CJC, BAMS:  bug-fix for Cray case; 
    bug discovered by David Wong, US EPA.

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED FC'S FOLLOW: **/

#if FLDMN

#define WRATT3 wratt3_
#define WRATTC wrattc_

#elif defined(__hpux) || defined(_AIX)

#define WRATT3 wratt3
#define WRATTC wrattc

#endif


#if defined(WRATT3) || defined(ABSFT)

    extern FINT WRATT3( const char * fname ,
                        const char * vname ,
                        const char * aname ,
                        const FINT * atype,
                        const FINT * amax,
                        void       * aval ,
                        FSTR_L       fnamelen, 
                        FSTR_L       vnamelen, 
                        FSTR_L       anamelen ) ;

    extern FINT WRATTC( const char * fname ,
                        const char * vname ,
                        const char * aname ,
                        char       * aval ,
                        FSTR_L       fnamelen, 
                        FSTR_L       vnamelen, 
                        FSTR_L       anamelen, 
                        FSTR_L       avallen ) ;

int wratt3c( const char  * fname ,
             const char  * vname ,
             const char  * aname ,
             int           atyp ,
             int           amax ,
             int         * asiz ,
             void        * att )

    {       /*  begin body of wratt3c() */
    FINT   atype, amax1 ;
    FSTR_L amaxc ;

    if ( atyp == M3CHAR )
        {
        amaxc = (FSTR_L) amax ;
        return( (FINT) WRATTC( fname ,
                               vname ,
                               aname ,
                               att   ,
                               STRLEN( fname ) ,
                               STRLEN( vname ) ,
                               STRLEN( aname ) ,
                               amaxc ) ) ;
        }
    else
        {
        atype = (FINT)   atyp ;
        amax1 = (FINT)   amax ;
        return( (FINT) WRATT3( fname ,
                               vname ,
                               aname ,
                             & atype ,
                             & amax1 ,
                               att  ,
                               STRLEN( fname ) ,
                               STRLEN( vname ) ,
                               STRLEN( aname ) ) ) ;
        }

    }       /*  end body of  wratt3c()  */


                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  Win32 wratt3c(): **/


#elif defined(_WIN32)


#include <stdio.h> /* for sprintf */

    extern FINT WRATT3( const char * fname , FSTR_L  fnamelen ,
                        const char * vname , FSTR_L  vnamelen ,
                        const char * aname , FSTR_L  anamelen ,
                        const FINT * atype,
                        const FINT * amax,
                        void       * aval ) ;

    extern FINT WRATTC( const char * fname , FSTR_L  fnamelen ,
                        const char * vname , FSTR_L  vnamelen ,
                        const char * aname , FSTR_L  anamelen ,
                        char       * aval  , FSTR_L  avallen ) ;


int wratt3c( const char  * fname ,
             const char  * vname ,
             const char  * aname ,
             int           atyp ,
             int           amax ,
             void        * att )

    {       /*  begin body of  wratt3c() */
    FINT   atype, amax1 ;
    FSTR_L amaxc ;
    
    if ( atyp == M3CHAR )
        {
        amaxc = (FSTR_L) amax ;
        return( (FINT) WRATTC( fname , STRLEN( fname ) ,
                        vname , STRLEN( vname ) ,
                        aname , STRLEN( aname ) ,
                        att   , amaxc ) ) ;
        }
    else{
        atype = (FINT)   atyp ;
        amax1 = (FINT)   amax ;
        return( (FINT) WRATT3( fname , STRLEN( fname ) ,
                               vname , STRLEN( vname ) ,
                               aname , STRLEN( aname ) ,
                             & atype ,
                             & amax1 ,
                               att ) ) ;
        }
    }       /*  end body of  wratt3c()  */


                	/** END  CASE OF Win32 **/
                	/** NEXT CASE:  CRAY-TARGETED wratt3c(): **/

#elif  defined(_CRAY)   /** treatment:  CRAY arryas of strings?? **/


#include <fortran.h>

    extern FINT WRATT3( const _fcd   fname ,
                        const _fcd   vname ,
                        const _fcd   aname ,
                        const FINT * atype ,
                        const FINT * amax  ,
                        void       * aval ) ;

    extern FINT WRATTC( const _fcd   fname ,
                        const _fcd   vname ,
                        const _fcd   aname ,
                        _fcd         aval ) ;

int wratt3c( const char  * fname ,
             const char  * vname ,
             const char  * aname ,
             int           atyp ,
             int           amax ,
             void        * att )

    {       /*  begin body of  wratt3c() */
    FINT   atype, amax1 ;
    FSTR_L amaxc ;
    _fcd  file ;
    _fcd  vble ;
    _fcd  atnm ;
    _fcd  aval ;

    file = _cptofcd( (char *)fname, STRLEN( fname ) ) ;
    vble = _cptofcd( (char *)vname, STRLEN( vname ) ) ;
    atnm = _cptofcd( (char *)aname, STRLEN( aname ) ) ;

    if ( atyp == M3CHAR )
        {
        amaxc = (FSTR_L) amax ;
        aval  = _cptofcd( (char *)att, amaxc ) ;
        return( _btol( WRATTC( file ,
                               vble ,
                               atnm ,
                               aval ) ) ) ;
        }
    else{
        atype = (FINT)   atyp ;
        amax1 = (FINT)   amax ;
        return( _btol( WRATT3( file ,
                               vble ,
                               atnm ,
                             & atype ,
                             & amax1 ,
                               att ) ) ) ;
        }

    }       /*  end body of  wratt3c()  */

#else

#error   "Error compiling wratt3c():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED "FC" TARGETED, 
                        OR IF Win32, OR IF CRAY **/



