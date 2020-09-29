
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API.
    "read4dc.c" version "$Id: read4dc.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT:
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    reads specified layer of variable "vname" from file with 
    logical name "fname" and puts it into "buffer".
    C wrapper around I/O API Fortran binding routine READ4D().


PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

CALLS:
    I/O API Fortran-binding routine READ4D()

REVISION HISTORY:
    Prototype 3/1995 by CJC

    Version   8/1999 by CJC:  FLDMN, Win32 portability enhancements

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.

**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN

#define READ4D read4d_

#elif defined(__hpux) || defined(_AIX)

#define READ4D read4d

#endif


#if defined(READ4D) || defined(ABSFT)

    extern FINT READ4D( const char * fname ,
                        const char * vname ,
                        FINT       * layer ,
                        FINT       * jdate ,
                        FINT       * jtime ,
                        FINT       * tstep ,
                        FINT       * nrecs ,
                        void       * buffer,
                        FSTR_L       fnamelen ,
                        FSTR_L       vnamelen ) ;

int read4dc( const char * fname ,
             const char * vname ,
             int          layer ,
             int          jdate ,
             int          jtime ,
             int          tstep ,
             int          nrecs ,
             void       * buffer )

    {       /*  begin body of read4dc() */
    FINT   lay, date, time, step, nrec ;

    lay  = (FINT) layer ;
    date = (FINT) jdate ;
    time = (FINT) jtime ;
    step = (FINT) tstep ;
    nrec = (FINT) nrecs ;

    return (int) READ4D(  fname , 
                          vname , 
                        & lay ,
                        & date , 
                        & time , 
                        & step , 
                        & nrec , 
                          buffer,
                          STRLEN( fname ) , 
                          STRLEN( vname ) ) ;

    }       /*  end body of read4dc ()  */

                	/** END  CASE OF FELDMAN-DESCENDED read4dc() **/
                	/** NEXT CASE:               WIN32 read4dc(): **/

#elif defined(_WIN32)

    extern FINT READ4D( const char * fname , FSTR_L  fnamelen ,
                        const char * vname , FSTR_L  vnamelen ,
                        FINT       * layer ,
                        FINT       * jdate ,
                        FINT       * jtime ,
                        FINT       * tstep ,
                        FINT       * nrecs ,
                        void       * buffer );

int read4dc( const char * fname ,
             const char * vname ,
             int          layer ,
             int          jdate ,
             int          jtime ,
             int          tstep ,
             int          nrecs ,
             void       * buffer )

    {       /*  begin body of read4dc() */
    FINT   lay, date, time, step, nrec ;

    lay  = (FINT) layer ;
    date = (FINT) jdate ;
    time = (FINT) jtime ;
    step = (FINT) tstep ;
    nrec = (FINT) nrecs ;

    return (int) READ4D(  fname , STRLEN( fname ) , 
                          vname , STRLEN( vname ) ,
                        & lay ,
                        & date , 
                        & time , 
                        & step , 
                        & nrec , 
                          buffer );

    }       /*  end body of read4dc ()  */

                	/** END  CASE OF WIN32 read4dc() **/
                	/** NEXT CASE:  CRAY CF77-TARGETED read4dc(): **/

#elif  defined(_CRAY)


#include <fortran.h>

    extern int READ4D( const _fcd    fname ,
                       const _fcd    vname ,
                       const  int  * layer ,
                       const  int  * jdate ,
                       const  int  * jtime ,
                       const  int  * tstep ,
                       const  int  * nrecs ,
                       void        * buffer ) ;

int read4dc( const char * fname ,
             const char * vname ,
             int          layer ,
             int          jdate ,
             int          jtime ,
             int          tstep ,
             int          nrecs ,
                   void * buffer )
 
    {       /*  begin body of read4dc() */
    _fcd  file ;
    _fcd  vble ;
    FINT   lay, date, time, step, nrec ;

    lay  = (FINT) layer ;
    date = (FINT) jdate ;
    time = (FINT) jtime ;
    step = (FINT) tstep ;
    nrec = (FINT) nrecs ;
    
    file = _cptofcd( (char *)fname, STRLEN( fname ) ) ;
    vble = _cptofcd( (char *)vname, STRLEN( vname ) ) ;

    return _btol( READ4D(  file , 
                           vble , 
                         & lay ,
                         & date , 
                         & time , 
                         & step , 
                         & nrec , 
                           buffer ) ) ; 
                     
    }       /*  end body of read4dc ()  */

                	/** END  CASE OF CRAY CF77-TARGETED read4dc(): **/

#else

#error   "Error compiling read4dc():  unsupported architecture"

#endif              /** #IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

