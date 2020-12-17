
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "nextimec.c" version "$Id: nextimec.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE
	Increment date&time jdate:jtime by time interval tstep
        Returns a normalized result

PRECONDITIONS
	jtime, tstep coded HHMMSS = 100 * ( 100 * hours + minutes ) + seconds
        jdate coded YYYYDDD = 1000 * year + julian-day

CALLS
	I/O API's Fortran-binding  NEXTIME()

REVISION HISTORY
	prototype  3/1995 byu CJC

	Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
        type resolution modifications

        Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
        upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  "iodecl3.h"


#if FLDMN
#    define    NEXTIME  nextime_
#elif defined(__hpux) || defined(_AIX)
#    define    NEXTIME  nextime
#elif  defined(_CRAY) || defined(_WIN32) || defined(ABSFT)
     /** do nothing **/
#else
#    error   "Error compiling init3c():  unsupported architecture"
#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/


    extern void  NEXTIME( FINT  * date, 
                          FINT  * time, 
                          FINT  * step ) ;


void nextimec ( int  * jdate,  int  * jtime, int tstep )
    {
    FINT   date, time, step ;

    date = (FINT) * jdate ;
    time = (FINT) * jtime ;
    step = (FINT)   tstep ;

    NEXTIME( & date, & time,  & step ) ;

    * jdate = (int) date ;
    * jtime = (int) time ;

    }       /*  end body of nextimec()  */
                	/** END  CASE OF CRAY CF77-TARGETED init3c(): **/


