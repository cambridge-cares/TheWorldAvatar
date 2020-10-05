/**************************************************************************
VERSION "$Id: dt2strc.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    format and return the date and time as a character string
    "HH:MM:SS  M+ D+, YYYY"

PRECONDITIONS:
    valid Julian date YYYYDDD, time HHMMSS

CALLS:  none

REVISION HISTORY:
    Prototype 3/1995 by Carlie J Coats, Jr, MCNC Environmental Programs
    Revised   8/1999 by CJC -- uses mmddyyc(), hhmmssc()
    Revised   2/2002 by CJC -- error messages via m3msgc()
    Revised   3/2018 by CJC -- standard-[week|month|year] handling is managed
                               by mmddyyc, hhmmsscs
**************************************************************************/

#include  <string.h>
#include  <stdio.h>
#include  "iodecl3.h"

void   dt2strc( int         jdate ,
                int         jtime ,
                char  buffer[ 25 ] )
{

int  col ;

if ( ( jdate == 0 ) && ( jtime == 0 ) )
    {
    strcpy( buffer, "date&time 0:0" ) ;
    return ;    
    }

hhmmssc( jtime, buffer ) ;
col = STRLEN( buffer ) ;
buffer[ col ] = ' ' ;
mmddyyc( jdate, buffer + col + 1 ) ;

return ; 
    
}		/** END BODY OF void dt2strc() **/

