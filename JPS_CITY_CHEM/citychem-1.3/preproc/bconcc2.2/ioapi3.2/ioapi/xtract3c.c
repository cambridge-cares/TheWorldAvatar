
/**************************************************************************
VERSION "$Id: xtract3c.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    reads specified 4D window of variable "vname" from file with 
    logical name "fname" and puts it into "buffer".
    C wrapper around I/O API Fortran binding routine XTRACT3().

RETURNS:  actual number of time steps read.

PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

CALLS:
    Fortran I/O API's XTRACT3(), NEXTIME().

REVISION HISTORY:
    Prototype 3/95 by CJC

    Version   8/99 by CJC:  FLDMN, Win32

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN       /*  see parms3.h  */

#define XTRACT3 xtract3_

#elif defined(__hpux) || defined(_AIX)

#define XTRACT3 xtract3

#endif


#if defined(XTRACT3) || defined(ABSFT)

    extern FINT XTRACT3( const char *fname , const char *vname ,
                         FINT       *lolay , FINT       *hilay ,
                         FINT       *lorow , FINT       *hirow ,
                         FINT       *locol , FINT       *hicol ,
                         FINT       *jdate , FINT       *jtime , 
                         void       *buffer,
                         FSTR_L      fnamelen ,
                         FSTR_L      vnamelen ) ;

int xtract3c( const char * fname , const char * vname ,
              int          lolay , int          hilay ,
              int          lorow , int          hirow ,
              int          locol , int          hicol ,
              int          jdate , int          jtime , 
              void       * buffer )

    {       /*  begin body of xtract3c() */
    FINT   lol, hil, lor, hir, loc, hic, date, time ;

    lol  = lolay ;
    hil  = hilay ;
    lor  = lorow ;
    hir  = hirow ;
    loc  = locol ;
    hic  = hicol ;
    date = jdate ;
    time = jtime ;
    
    return (int) XTRACT3(  fname , vname , 
                         & lol , & hil ,
                         & lor , & hir ,
                         & loc , & hic ,
                         & date, & time, 
                         buffer,
                         STRLEN( fname ) , 
                         STRLEN( vname ) ) ;

    }       /*  end body of xtract3c ()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  Win32 xtract3c(): **/

#elif defined(_WIN32)

    extern FINT XTRACT3( const char *fname , FSTR_L fnamelen ,
                         const char *vname , FSTR_L vnamelen ,
                         FINT       *lolay , FINT  *hilay ,
                         FINT       *lorow , FINT  *hirow ,
                         FINT       *locol , FINT  *hicol ,
                         FINT       *jdate , FINT  *jtime , 
                         void       *buffer );

int xtract3c( const char * fname , const char * vname ,
              int          lolay , int          hilay ,
              int          lorow , int          hirow ,
              int          locol , int          hicol ,
              int          jdate , int          jtime , 
              void       * buffer )

    {       /*  begin body of xtract3c() */ 
    FINT   lol, hil, lor, hir, loc, hic, date, time ;

    lol  = lolay ;
    hil  = hilay ;
    lor  = lorow ;
    hir  = hirow ;
    loc  = locol ;
    hic  = hicol ;
    date = jdate ;
    time = jtime ;

    return (int) XTRACT3(  fname , STRLEN( fname ) ,
                           vname , STRLEN( vname ) ,
                         & lol , & hil ,
                         & lor , & hir ,
                         & loc , & hic ,
                         & date, & time , 
                         buffer );

    }       /*  end body of xtract3c ()  */

                	/** END  CASE OF Win32 xtract3c() **/
                	/** NEXT CASE:  CRAY CF77-TARGETED xtract3c(): **/


#elif  defined(_CRAY)


#include <fortran.h>

    extern  int XTRACT3( const _fcd  fname , const _fcd  vname ,
                         FINT       *lolay , FINT       *hilay ,
                         FINT       *lorow , FINT       *hirow ,
                         FINT       *locol , FINT       *hicol ,
                         FINT       *jdate , FINT       *jtime ,  
                         void       *buffer ) ;

int xtract3c( const char * fname , const char * vname ,
              int          lolay , int          hilay ,
              int          lorow , int          hirow ,
              int          locol , int          hicol ,
              int          jdate , int          jtime , 
              void       * buffer )
 
    {       /*  begin body of xtract3c() */
    FINT   lol, hil, lor, hir, loc, hic, date, time ;    
    _fcd   file ;
    _fcd   vble ;
    
    lol  = lolay ;
    hil  = hilay ;
    lor  = lorow ;
    hir  = hirow ;
    loc  = locol ;
    hic  = hicol ;
    date = jdate ;
    time = jtime ;
    file = _cptofcd( (char *)fname, STRLEN( fname ) ) ;
    vble = _cptofcd( (char *)vname, STRLEN( vname ) ) ;

    return _btol( XTRACT3(  file,   vble , 
                          & lol , & hil ,
                          & lor , & hir ,
                          & loc , & hic ,
                          & date, & time ,  buffer ) ) ; 
                     
    }       /*  end body of xtract3c ()  */

                	/** END  CASE OF CRAY CF77-TARGETED xtract3c(): **/

#else

#error   "Error compiling xtract3c():  unsupported architecture"

#endif              /** #IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

