
/**************************************************************************
VERSION "$Id: hhmmssc.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    format and return the time as a character string
    "HH:MM:SS  M+ D+, YYYY"

PRECONDITIONS:
    valid time HHMMSS

CALLS:
    none

REVISION HISTORY:
    Prototype 3/1995 by Carlie J Coats, Jr, MCNC Environmental Programs

    Revised   2/2002 by CJC -- error messages via m3msgc()
**************************************************************************/

#include  <string.h>
#include  <stdio.h>
#include  "iodecl3.h"

void   hhmmssc( int   jtime ,
                char  buffer[ 11 ] )
{
int   hour, mins, secs ;
char  mesg[256] ;

if ( jtime > 999999 || jtime < 0 ) 
    {
    sprintf( mesg, 
             "%s %d",
             "Time-number error in hhmmssc():  jtime = ", jtime ) ;
    m3mesgc( mesg ) ;
    strcpy( buffer, "<TIMERROR>" ) ;
    return ;
    }

hour = jtime ;
secs = hour % 100 ;
hour = hour / 100 ;
mins = hour % 100 ;
hour = hour / 100 ;

sprintf( buffer, "%02d:%02d:%02d\0", hour , mins , secs ) ;
return ;
    
}		/** END BODY OF void hhmmssc() **/

