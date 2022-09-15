
/**************************************************************************
VERSION "$Id: secsdiffc.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE
    returns the time interval (signed seconds) from
    ADATE:ATIME to ZDATE:ZTIME

PRECONDITIONS
    normalized dates and times (0 <= SS <= 59, etc.)
    stored in format YYYYDDD:HHMMSS.

CALLS
    none

REVISION HISTORY
    prototype  3/1995 by Carlie J. Coats, Jr.,
    MCNC Environmental Modeling Center

    Unification 2/2002 with Global Climate Model IO_360 version,
    that uses a 360-day year

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Version 4/2016 by CJC:  add Global Climate Model IO_365 version
**************************************************************************/

#include  "iodecl3.h"


int secsdiffc( int  adate , 
               int  atime , 
               int  zdate , 
               int  ztime )
    {
    int  days, hours, mins, secs, total ;

/** start with day, hour, min, sec diffe **/
    
    days  =   zdate % 1000         -    adate % 1000 ;
    hours =   ztime / 10000        -    atime / 10000 ;
    mins  = ( ztime / 100 ) % 100  -  ( atime / 100 ) % 100 ;
    secs  =   ztime % 100          -    atime % 100 ;
    total = 60 * ( 60 * ( 24 * days + hours ) + mins ) + secs ;
    
/** Now add corrections for differences in years **/
    
#ifdef IO_360

    for ( adate/=1000 ,  zdate/=1000 ; adate < zdate ; adate++ )
        {
        total += 360 * 86400 ;
        }

    for ( ; zdate < adate ; zdate++ )
        {
        total -= 360 * 86400 ;
        }

    return  total ;
    
#endif
    
#ifdef IO_365
    
    for ( adate/=1000 ,  zdate/=1000 ; adate < zdate ; adate++ )
        {
        total += 365 * 86400 ;
        }

    for ( ; zdate < adate ; zdate++ )
        {
        total -= 365 * 86400 ;
        }

    return  total ;

#endif
    

    for ( adate/=1000 ,  zdate/=1000 ; adate < zdate ; adate++ )
        {
        if      ( adate %   4 ) total += 365 * 86400 ;
        else if ( adate % 100 ) total += 366 * 86400 ;
        else if ( adate % 400 ) total += 365 * 86400 ;
        else                    total += 366 * 86400 ;
        }

    for ( ; zdate < adate ; zdate++ )
        {
        if      ( zdate %   4 ) total -= 365 * 86400 ;
        else if ( zdate % 100 ) total -= 366 * 86400 ;
        else if ( zdate % 400 ) total -= 365 * 86400 ;
        else                    total -= 366 * 86400 ;
        }

    return  total ;

    }       /*  end body of secsdiffc()  */

