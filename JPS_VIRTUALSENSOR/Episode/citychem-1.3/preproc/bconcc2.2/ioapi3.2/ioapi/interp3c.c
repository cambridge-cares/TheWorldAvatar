
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "interp3c.c" version "$Id: interp3c.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
 	Time-interpolate specified variable "vname" from file with 
  	logical name "fname" from "buffer", with optional windowing
        for INTERPX().
 	C wrappers around I/O API Fortran binding routines INTERP3()
        and INTERPX().
 	
 
PRECONDITIONS:
 	FNAME already opened by OPEN3() or open3c()
 	VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'
 	Valid dimension bounds.
 
CALLS:
 	I/O API's Fortran-bindings INTERP3() and INTERPX()
 
REVISION HISTORY:
 	Prototype 3/1995 by Carlie J. Coats, Jr., MCNC Environmental Programs
 	Version   8/1999 by CJC:  WIN32 stuff
        Version   2/2002 by CJC:  add windowing INTERPX() functionality.
 	Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
        type resolution modifications

        Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
        upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN
#define INTERP3 interp3_
#define INTERPX interpx_
#elif defined(__hpux) || defined(_AIX)
#define INTERP3 interp3
#define INTERPX interpx
#endif


#if defined(INTERP3) || defined(ABSFT)

		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

    extern FINT  INTERP3( const char  * fname ,
                          const char  * vname ,
                          const char  * cname ,
                          FINT         * jdate ,
                          FINT         * jtime ,
                          FINT         * bsize ,
                          FREAL       * buffer,
                          FSTR_L        fnamelen ,
                          FSTR_L        vnamelen ,
                          FSTR_L        cnamelen ) ;

    extern FINT  INTERPX( const char  * fname ,
                          const char  * vname ,
                          const char  * cname ,
                          FINT        * col0 ,
                          FINT        * col1 ,
                          FINT        * row0 ,
                          FINT        * row1 ,
                          FINT        * lay0 ,
                          FINT        * lay1 ,
                          FINT        * jdate ,
                          FINT        * jtime ,
                          FREAL       * buffer,
                          FSTR_L        fnamelen ,
                          FSTR_L        vnamelen ,
                          FSTR_L        cnamelen ) ;

int interp3c( const char  * fname ,
              const char  * vname ,
              const char  * cname ,
              int           jdate ,
              int           jtime ,
              int           bsize ,
              FREAL       * buffer )

    {       /*  begin body of interp3c() */

    FINT date, time, size ;

    date = (FINT) jdate ;
    time = (FINT) jtime ;
    size = (FINT) bsize ;

    return INTERP3(  fname , 
                     vname , 
                     cname , 
                   & date , 
                   & time , 
                   & size , 
                     buffer,
                     STRLEN( fname ) , 
                     STRLEN( vname ) , 
                     STRLEN( cname ) ) ;

    }       /*  end body of interp3c()  */

int interpxc( const char  * fname ,
              const char  * vname ,
              const char  * cname ,
              int           c0 ,
              int           c1 ,
              int           r0 ,
              int           r1 ,
              int           l0 ,
              int           l1 ,
              int           jdate ,
              int           jtime ,
              FREAL       * buffer )

    {       /*  begin body of interp3c() */

    FINT date, time, col0, col1, row0, row1, lay0, lay1 ;

    date = (FINT) jdate ;
    time = (FINT) jtime ;
    col0 = (FINT) c0 ;
    col1 = (FINT) c1 ;
    row0 = (FINT) r0 ;
    row1 = (FINT) r1 ;
    lay0 = (FINT) l0 ;
    lay1 = (FINT) l1 ;

    return INTERPX(  fname , 
                     vname , 
                     cname , 
                   & col0 , 
                   & col1 , 
                   & row0 , 
                   & row1 , 
                   & lay0 , 
                   & lay1 , 
                   & date , 
                   & time , 
                     buffer,
                     STRLEN( fname ) , 
                     STRLEN( vname ) , 
                     STRLEN( cname ) ) ;

    }       /*  end body of interp3c()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  WIN32-TARGETED interp3c(): **/

#elif defined(_WIN32)

    extern FINT  INTERP3( const char  * fname ,
                          FSTR_L        fnamelen ,
                          const char  * vname ,
                          FSTR_L        vnamelen ,
                          const char  * cname ,
                          FINT          cnamelen ,
                          FINT        * jdate ,
                          FINT        * jtime ,
                          FINT        * bsize ,
                          FREAL       * buffer );

    extern FINT  INTERPX( const char  * fname ,
                          int           fnamelen ,
                          const char  * vname ,
                          int           vnamelen ,
                          const char  * cname ,
                          FINT          cnamelen ,
                          FINT        * col0 ,
                          FINT        * col1 ,
                          FINT        * row0 ,
                          FINT        * row1 ,
                          FINT        * lay0 ,
                          FINT        * lay1 ,
                          FINT        * jdate ,
                          FINT        * jtime ,
                          FREAL       * buffer );

int interp3c( const char  * fname ,
              const char  * vname ,
              const char  * cname ,
              int           jdate ,
              int           jtime ,
              int           bsize ,
              FREAL       * buffer )

    {       /*  begin body of interp3c() */

    FINT date, time, size ;

    date = (FINT) jdate ;
    time = (FINT) jtime ;
    size = (FINT) bsize ;

    return INTERP3(  fname , 
                     STRLEN( fname ) , 
                     vname , 
                     STRLEN( vname ) ,
                     cname , 
                     STRLEN( cname ) ,
                   & date , 
                   & time , 
                   & size , 
                     buffer );

    }       /*  end body of interp3c()  */

int interpxc( const char  * fname ,
              const char  * vname ,
              const char  * cname ,
              int           c0 ,
              int           c1 ,
              int           r0 ,
              int           r1 ,
              int           l0 ,
              int           l1 ,
              int           jdate ,
              int           jtime ,
              FREAL       * buffer )

    {       /*  begin body of interp3c() */

    FINT date, time, col0, col1, row0, row1, lay0, lay1 ;

    date = (FINT) jdate ;
    time = (FINT) jtime ;
    col0 = (FINT) c0 ;
    col1 = (FINT) c1 ;
    row0 = (FINT) r0 ;
    row1 = (FINT) r1 ;
    lay0 = (FINT) l0 ;
    lay1 = (FINT) l1 ;

    return INTERPX(  fname , 
                     STRLEN( fname ) , 
                     vname , 
                     STRLEN( vname ) ,
                     cname , 
                     STRLEN( cname ) ,
                   & col0 , 
                   & col1 , 
                   & row0 , 
                   & row1 , 
                   & lay0 , 
                   & lay1 , 
                   & date , 
                   & time , 
                     buffer );

    }       /*  end body of interp3c()  */

                	/** END  CASE OF WIN32 F77 TARGETS **/
                	/** NEXT CASE:  CRAY CF77-TARGETED interp3c(): **/

#elif  defined(_CRAY)


#include <fortran.h>

    extern FINT  INTERP3( const _fcd     fname ,
                          const _fcd     vname ,
                          const _fcd     cname ,
                          const  FINT   * jdate ,
                          const  FINT   * jtime ,
                          const  FINT   * bsize ,
                          FREAL        * buffer ) ;

    extern FINT  INTERPX( const _fcd     fname ,
                          const _fcd     vname ,
                          const _fcd     cname ,
                          const  FINT  * col0 ,
                          const  FINT  * col1 ,
                          const  FINT  * row0 ,
                          const  FINT  * row1 ,
                          const  FINT  * lay0 ,
                          const  FINT  * lay1 ,
                          const  FINT  * jdate ,
                          const  FINT  * jtime ,
                          FREAL        * buffer ) ;

int interp3c( const char  * fname ,
              const char  * vname ,
              const char  * cname ,
              int           jdate ,
              int           jtime ,
              int           bsize ,
              FREAL       * buffer )
 
    {       /*  begin body of interp3c() */
    
    FINT date, time, size ;

    _fcd  file ;
    _fcd  vble ;
    _fcd  caller ;
    
    date = (FINT) jdate ;
    time = (FINT) jtime ;
    size = (FINT) bsize ;

    file   = _cptofcd( (char *)fname, STRLEN( fname ) ) ;
    vble   = _cptofcd( (char *)vname, STRLEN( vname ) ) ;
    caller = _cptofcd( (char *)cname, STRLEN( cname ) ) ;

    return _btol( INTERP3(  file  , 
                            vble  , 
                            caller  , 
                          & date , 
                          & time ,
                          & size ,
                            buffer ) ) ; 
                     
    }       /*  end body of interp3c ()  */

int interpxc( const char  * fname ,
              const char  * vname ,
              const char  * cname ,
              int           c0 ,
              int           c1 ,
              int           r0 ,
              int           r1 ,
              int           l0 ,
              int           l1 ,
              int           jdate ,
              int           jtime ,
              FREAL       * buffer )
 
    {       /*  begin body of interp3c() */
    
    FINT date, time, col0, col1, row0, row1, lay0, lay1 ;

    _fcd  file ;
    _fcd  vble ;
    _fcd  caller ;
    
    date = (FINT) jdate ;
    time = (FINT) jtime ;
    col0 = (FINT) c0 ;
    col1 = (FINT) c1 ;
    row0 = (FINT) r0 ;
    row1 = (FINT) r1 ;
    lay0 = (FINT) l0 ;
    lay1 = (FINT) l1 ;

    file   = _cptofcd( (char *)fname, STRLEN( fname ) ) ;
    vble   = _cptofcd( (char *)vname, STRLEN( vname ) ) ;
    caller = _cptofcd( (char *)cname, STRLEN( cname ) ) ;

    return _btol( INTERPX(  file  , 
                            vble  , 
                            caller  , 
                          & col0 , 
                          & col1 , 
                          & row0 , 
                          & row1 , 
                          & lay0 , 
                          & lay1 , 
                          & date , 
                          & time , 
                            buffer ) ) ; 
                     
    }       /*  end body of interpxc ()  */

                	/** END  CASE OF CRAY CF77-TARGETED interp3c(): **/

#else

#error   "Error compiling interp3c():  unsupported architecture"

#endif              /** #IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

