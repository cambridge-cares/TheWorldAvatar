/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "julianc.c" version "$Id: julianc.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE
    Return Julian day (1...365,366) corresponding to the date
    <month - dayofmonth - year>
    NOTE:  This is NOT the Julian DATE -- only the day-number.
    To get the Julian date:

       jdate = 1000 * (year % 100)  +  julianc( year , mnth , mday )

PRECONDITIONS

CALLS
    none

REVISION HISTORY
    prototype  3/1995 by Carlie J. Coats, Jr.,
    MCNC Environmental Modeling Center

    Unification 2/2002 with Global Climate Model IO_360 version,
    that uses a 360-day GC-year.

    Version 4/2016 by CJC:  add Global Climate Model IO_365 version
                    
**************************************************************************/

#include  "iodecl3.h"


int julianc ( int year, int month, int mday )
    {
    int  k, m, n ;
    
#ifdef IO_360
    return  mday + 30 * ( month - 1 ) ;
#endif

#ifdef IO_365
    m = ( month + 9 ) % 12 ;
    n = (m * 153 + 2) / 5 + mday + 58 ;
    return  1  +  n % 365 ;
#endif

    m = ( month + 9 ) % 12 ;
    n = (m * 153 + 2) / 5 + mday + 58 ;
    k = ( ( year %   4 ) ? 365 :
        ( ( year % 100 ) ? n++, 366 :
        ( ( year % 400 ) ? 365 : n++, 366 ) ) ) ;
    return  1  +  n % k ;

    }       /*  end body of julianc()  */

