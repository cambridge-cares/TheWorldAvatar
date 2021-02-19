/**************************************************************************
VERSION "$Id: currstepc.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE
    If possible, compute the date&time cdate:ctime for the time 
    step in the time step sequence starting at sdate:stime and 
    having the time step tstep and return TRUE; 
    otherwise, return FALSE.  

    In particular, cdate:ctime is the date&time of largest time step
    in the sequence having the property:

        cdate:ctime <= jdate:jtime

    Return TRUE with cdate=sdate, ctime=stime if tstep==0.

PRECONDITIONS
    normalized dates and times (0 <= SS <= 59, etc.)
    stored in format YYYYDDD:HHMMSS.

CALLS
    none

REVISION HISTORY
    prototype 3/1995 by CJC

    revised   9/1999 by CJC for I/O API v2:  add WIN32

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.

**************************************************************************/

#include  "iodecl3.h"
#include  <stdlib.h>

#if FLDMN

#define   CURRSTEP currstep_ 

   extern FINT  CURRSTEP( FINT*, FINT*, FINT*, FINT*, FINT*, FINT*, FINT* );

#elif defined(__hpux) || defined(_AIX)

#define CURRSTEP              currstep

   extern FINT  CURRSTEP( FINT*, FINT*, FINT*, FINT*, FINT*, FINT*, FINT* );

#elif defined( _WIN32)

   extern FINT __stdcall CURRSTEP( FINT*, FINT*, FINT*, FINT*, FINT*, FINT*, FINT* );

#elif  defined(_CRAY) || defined(ABSFT)

   extern FINT  CURRSTEP( FINT*, FINT*, FINT*, FINT*, FINT*, FINT*, FINT* );

#else
 
#error   "Error compiling:  unsupported architecture"
 
#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/


int currstepc( int  jdate , int  jtime , 
               int  sdate , int  stime , int  tstep ,
               int *cdate , int *ctime )
    {
    FINT jd, jt, sd, st, ts, cd, ct ;
    
    jd = jdate ;
    jt = jtime ;
    sd = sdate ;
    st = stime ;
    ts = tstep ;

    if ( (int) CURRSTEP( &jd, &jt, &sd, &st, &ts, &cd, &ct ) )
        {
        *cdate = cd ;
        *ctime = ct ;
        return( 1 ) ;
        }
    else{
        return( 0 ) ;
        }

    }       /*  end body of currstepc()  */

