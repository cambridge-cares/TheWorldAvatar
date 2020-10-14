/**************************************************************************
VERSION "$Id: daymonc.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE
    Determines the month (1...12) and day of the month (1..31)
    for the Julian date YYYYDDD that is input

PRECONDITIONS

CALLS
    none

REVISION HISTORY
    prototype   3/1995 by Carlie J. Coats, Jr.,
    MCNC Environmental Modeling Center

    Unification 2/2002 with Global Climate Model IO_360 version,
    that uses a 360-day year

    Version 4/2016 by CJC:  add Global Climate Model IO_365 version

**************************************************************************/

#include  "iodecl3.h"


void daymonc ( int jdate, int  *month, int  *mday )
    {
    int  year, day, j, k ;
     
#if  defined( IO_360 )

    day  = jdate % 1000 - 1 ;
    
    *month = day / 30  +  1 ;
    *mday  = day % 30  +  1 ;

#elif  defined( IO_365 )

    year = jdate / 1000 ;
    day  = jdate % 1000 ;
    j    = ( day + 305 ) % 365 ;
    j    = ( j % 153 ) / 61  +  2 * ( j / 153 )  +  j ;
    *month = ( j / 31 + 2 ) % 12  +  1 ;
    *mday  =   j % 31             +  1 ;

#else

    year = jdate / 1000 ;
    day  = jdate % 1000 ;
    k = ( year %   4 ) ? 365 :
        ( year % 100 ) ? 366 :
        ( year % 400 ) ? 365 : 366 ;
    j = ( day + 305 ) % k ;
    j = ( j % 153 ) / 61  +  2 * ( j / 153 )  +  j ;
    
    *month = ( j / 31 + 2 ) % 12  +  1 ;
    *mday  =   j % 31             +  1 ;

#endif

    }       /*  end body of daymonc()  */

