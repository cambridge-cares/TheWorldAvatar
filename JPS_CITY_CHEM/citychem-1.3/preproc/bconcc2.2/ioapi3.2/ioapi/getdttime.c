/**************************************************************************
VERSION "$Id: getdttime.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    get current date and time from system using  time()

PRECONDITIONS:
    current date before 2038, when  "time()"  fails on 32-bit machines.

CALLS:
    standard library function  "time()"

REVISION HISTORY:
    prototype  5/1991 by Carlie J. Coats, Jr.,
    MCNC Environmental Modeling Center

    Unification 2/2002 with Global Climate Model IO_360 version,
    that uses a 360-day year

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 2/2007 by CJC:  "#include <stdlib.h>" for "exit()" call
    per Dr. Frank Binkowski
**************************************************************************/


#include <stdlib.h>
#include <time.h>
#include  "parms3.h"

#if FLDMN
#define GETDTTIME getdttime_
#elif defined(__hpux) || defined(_AIX)
#define GETDTTIME getdttime
#endif

void GETDTTIME ( FINT *now_date, FINT *now_time )

    {                                  /*  begin body of getdttime() */
    int      yrs, days, hrs, mins, secs ;
    time_t   elap ;

    /** time() returns seconds since basetime  00:00:00 Jan. 1, 1970  **/
    /**        or  0  for failure, as a  time_t == unsigned long      **/

    if ( ! ( elap = time ( (time_t *) 0 ) ) )
        {
        exit ( 2 ) ;
        } /**  END IF-BLOCK:  call to  time() failed **/


    secs = (int)( elap % 60 ) ;
    elap /= 60 ;     /** now in  minutes **/

    mins = (int)( elap % 60 ) ;
    elap /= 60 ;     /** now in  hours   **/

    hrs  = (int)( elap % 24 ) ;
    elap /= 24 ;     /** now in  days **/

    *now_time = (FINT)( 100 * ( 100 * hrs  +  mins )  +  secs ) ;


    for ( yrs = 1970 ; ; ) /** loop unrolled for leap-year cycle **/
        {
        if ( elap < 365 ) /** year == 2 mod 4 (e.g., 1970 itself) **/
            {
            days = elap + 1 ;
            break ;
            }         /**  END IF-BLOCK testing elap  **/
        else
            {
            elap -= 365 ;
            yrs  += 1   ;
            }         /**  END ELSE-BLOCK incrementing year(==2) and day  **/

        if ( elap < 365 ) /** year == 3 mod 4 **/
            {
            days = elap + 1 ;
            break ;
            }         /**  END IF-BLOCK testing elap  **/
        else
            {
            elap -= 365 ;
            yrs  += 1   ;
            }         /**  END ELSE-BLOCK incrementing year(==3) and day  **/

        days = ( yrs%100 ? 366 : ( yrs%400 ? 365 : 366 ) ) ;
        if ( elap < days )
            {                     /** year == 0 mod 4:  process ?leap years  **/
            days = elap + 1 ;     /** NOTE:  noncentury years are leap years **/
            break ;               /** century years iff divisible by 400     **/
            }         /**  END IF-BLOCK testing elap  **/
        else
            {
            elap -= days ;
            yrs  += 1   ;
            }         /**  END ELSE-BLOCK incrementing year(==0) and day  **/

        if ( elap < 365 ) /** year == 3 mod 4 **/
            {
            days = elap + 1 ;
            break ;
            }         /**  END IF-BLOCK testing elap  **/
        else
            {
            elap -= 365 ;
            yrs  += 1   ;
            }         /**  END ELSE-BLOCK incrementing year(==3) and day  **/

        }         /**  END FOR-LOOP calculating yrs and days **/


    *now_date = (FINT)( 1000 * yrs  +  days ) ;

    return ;

    }                                  /*  end body of getdttime()  */

