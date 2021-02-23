
/**************************************************************************
VERSION "$Id: time2secc.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE
	convert back and forth between Models-3 time conventions and 
	seconds

PRECONDITIONS
	time coded HHMMSS = 100 * ( 100 * hours + minutes ) + seconds

CALLS
	none

REVISION HISTORY
	prototype  3/95 by CJC

**************************************************************************/

#include  "iodecl3.h"


int time2secc ( int time )
    {
    int  secs ;
    if ( time >= 0 )
        {
        secs = time%100 + 60 * ( 60 * ( time / 10000 )  
                               +  ( time / 100 ) % 100 ) ;
        }
    else{
        secs = -time ;
        secs = -( secs%100 + 60 * ( 60 * ( secs / 10000 )  
                                  +  ( secs / 100 ) % 100 ) ) ;
        }
    
    return  secs ;

    }       /*  end body of time2secc()  */

