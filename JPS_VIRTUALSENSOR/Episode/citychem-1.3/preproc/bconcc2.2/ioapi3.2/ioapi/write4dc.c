
/**************************************************************************
VERSION "$Id: write4dc.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    Writes specified variable "vname" to file with 
    logical name "fname" from "buffer".
    C wrapper around I/O API Fortran binding routine WRITE3().


PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

CALLS:
    FI/O API's ortran binding WRITE3()

REVISION HISTORY:
    Prototype 3/95 by CJC

    Version    8/99 by CJC:  FLDMN, WIN32 portability enhancements

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN

#define WRITE4D write4d_

#elif defined(__hpux) || defined(_AIX)

#define WRITE4D write4d

#endif


#if defined(WRITE4D) || defined(ABSFT)

    extern FINT WRITE4D( const char *fname ,
                         const char *vname ,
                         FINT       *jdate ,
                         FINT       *jtime ,
                         FINT       *tstep ,
                         FINT       *nrecs ,
                         const void *buffer,
                         FSTR_L      fnamelen ,
                         FSTR_L      vnamelen ) ;

int write4dc( const char * fname ,
              const char * vname ,
              int          jdate ,
              int          jtime ,
              int          tstep ,
              int          nrecs ,
              const void * buffer )

    {       /*  begin body of write4dc() */
    FINT  date, time, step, nrec ;

    date = (FINT) jdate ;
    time = (FINT) jtime ;
    step = (FINT) tstep ;
    nrec = (FINT) nrecs ;

    return (int) WRITE4D(  fname , 
                           vname , 
                         & date , 
                         & time , 
                         & step , 
                         & nrec , 
                           buffer,
                           STRLEN( fname ) , 
                           STRLEN( vname ) ) ;

    }       /*  end body of write4dc ()  */

                	/** END  CASE OF FELDMAN-DESCENDED write4dc() **/
                	/** NEXT CASE:  Win32 write4dc(): **/

#elif defined(_WIN32)

    extern FINT WRITE4D( const char *fname , FSTR_L      fnamelen ,
                         const char *vname , FSTR_L      vnamelen ,
                         FINT       *jdate ,
                         FINT       *jtime ,
                         FINT       *tstep ,
                         FINT       *nrecs ,
                         const void *buffer ) ;

int write4dc( const char * fname ,
              const char * vname ,
              int          jdate ,
              int          jtime ,
              int          tstep ,
              int          nrecs ,
              const void * buffer )

    {       /*  begin body of write4dc() */
    FINT  date, time, step, nrec ;

    date = (FINT) jdate ;
    time = (FINT) jtime ;
    step = (FINT) tstep ;
    nrec = (FINT) nrecs ;

    return (int) write4d(  fname , STRLEN( fname ) , 
                           vname ,  STRLEN( vname ) ,
                         & date , 
                         & time , 
                         & step , 
                         & nrec , 
                           buffer ) ;

    }       /*  end body of write4dc ()  */

                	/** END  CASE OF Win32 write4dc() **/
                	/** NEXT CASE:  CRAY CF77-TARGETED write4dc(): **/

#elif  defined(_CRAY)


#include <fortran.h>

    extern FINT WRITE4D( const _fcd    fname ,
                         const _fcd    vname ,
                         const  FINT * jdate ,
                         const  FINT * jtime ,
                         const  FINT * tstep ,
                         const  FINT * nrecs ,
                         const  void * buffer ) ;

int write4dc( const char  * fname ,
              const char  * vname ,
              int           jdate ,
              int           jtime ,
              int           tstep ,
              int           nrecs ,
              const void  * buffer )
 
    {       /*  begin body of write4dc() */
    FINT  date, time, step, nrec ;
    _fcd  file ;
    _fcd  vble ;

    date = (FINT) jdate ;
    time = (FINT) jtime ;
    step = (FINT) tstep ;
    nrec = (FINT) nrecs ;

    file = _cptofcd( (char *)fname, STRLEN( fname ) ) ;
    vble = _cptofcd( (char *)vname, STRLEN( vname ) ) ;

    return _btol( WRITE4D(  file , 
                            vble , 
                          & date, 
                          & time,
                          & step,
                          & nrec,
                            buffer ) ) ; 
                     
    }       /*  end body of write4dc ()  */

                	/** END  CASE OF CRAY CF77-TARGETED write4dc(): **/

#else

#error   "Error compiling write4dc():  unsupported architecture"

#endif              /** #IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

