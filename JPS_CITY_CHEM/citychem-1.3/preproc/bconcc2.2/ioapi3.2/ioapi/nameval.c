
/***************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "locatsc.c" version "$Id: nameval.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE

  void nameval():  FORTRAN-callable shell around getenv()

  ---->  MACHINE-DEPENDENT !! <----

  find the value of environment variable "lname" in the environment
  and return it in "eqname"

PRECONDITIONS:
    len(lname) < BUFLEN = 512.
    case sensitivity handled correctly

CALLS:
    system call getenv()

REVISION HISTORY:
    Prototype 3/1991 by CJC

    Version   8/1999 by CJC for I/O APIv2:  FLDMN, WIN32
    portability enhancements

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
***************************************************************/

#include <stdio.h>
#include <stdlib.h>

#ifdef _AIX
/*  HACK to ensure that AIX links in all the right stuff.
    NAMEVAL() is sure to be linked into *any* IO API executable  */
#include <pthread.h>
#endif

#include "parms3.h"

#define  BUFLEN  65535

                /** CODE DEPENDS (BADLY) UPON HOW THE FORTRAN    **/
                /** COMPILER DEALS WITH NAMES AND WITH CHARACTER **/
                /** STRINGS:  2-CASES HERE:  FELDMAN-DESCENDED   **/
                /** F77'S, AND CRAY CF77'S TARGETED AS CALLERS   **/
                /** OF NAMEVAL().  FIRST CASE:  FELDMANISMS:     **/

#if defined(__hpux) || defined(_AIX)  /** Hack for HP, IBM f77's follows: **/

#define NAMEVAL nameval

#elif FLDMN

#define NAMEVAL nameval_

#endif


#if defined(NAMEVAL) || defined(ABSFT)

void NAMEVAL ( char * lname, 
               char * eqname,
               FSTR_L llen,
               FSTR_L elen )
    {
    char   buffer[ BUFLEN ] ;
    char  *source, *target , *bound, *limit, ch ;
    FSTR_L length ;

        /** COPY  lname  INTO  buffer[]  AS A C-STRING **/
        /** TRIM TRAILING BLANKS, AND ADD TERMINAL NULL **/
        /** FOR VMS, UPCASE AS YOU GO **/

    source = lname ;
    length = llen ;
    target = buffer ;
    bound  = target + ( length < BUFLEN-1 ? length : BUFLEN-1 ) ;
    for ( ; target < bound ; target++ , source++ )
        {
        ch = *source ;
        if ( ch == ' ' ) 
            {
            break ;
            } /**  END IF-CLAUSE:  BLANK CHARACTER **/
        else *target = ch ;

        } /**  END FOR-LOOP COPYING  lname  TO  buffer[], ETC.  **/

        *target = '\0' ; 


        /** COPY VALUE FROM ENVIRONMENT INTO CONTENTS OF eqname **/

    target = eqname ;
    bound  = target + elen ;
    if ( source = getenv( buffer ) )
        {
        for (   ; *target = *source ; target++ , source++ )
            {
            if ( target >= bound ) break ;
            }   /** END FOR-LOOP copying value from environment to eqname **/

        } /**  END IF-CLAUSE:  getenv() succeeded**/
    else
        {
        source = lname ;
        limit  = target + llen ;
        limit  = ( limit < bound ? limit : bound ) ;
        for (  ; target < limit ; *target++ = *source++ )  ; /** EMPTY BODY **/

        } /**  END END ELSE-CLAUSE:  getenv() failed **/

        /** PAD FORTRAN OUTPUT STRING WITH TRAILING BLANKS:  **/

    for (   ; target < bound ; target++ )  *target = ' ' ;

    } /**  END void function nameval() **/



                /** END  CASE OF FELDMAN-DESCENDED F77 TARGETS FOR USE   **/
                /** NEXT CASE:  WIN32 NAMEVAL(): **/


#elif  defined(_WIN32)

#include <stdlib.h>
void NAMEVAL ( char * lname, FSTR_L llen, char * eqname, FSTR_L elen )
    {
    char buffer[ BUFLEN ] ;
    char *source, *target , *bound, *limit, ch ;
    int length ;

        /** COPY  lname  INTO  buffer[]  AS A C-STRING **/
        /** TRIM TRAILING BLANKS, AND ADD TERMINAL NULL **/
        /** FOR VMS, UPCASE AS YOU GO **/

    source = lname ;
    length = llen ;
    target = buffer ;
    bound  = target + ( length < BUFLEN-1 ? length : BUFLEN-1 ) ;
    for ( ; target < bound ; target++ , source++ )
        {
        ch = *source ;
        if ( ch == ' ' ) 
            {
            break ;
            } /**  END IF-CLAUSE:  BLANK CHARACTER **/
        else *target = ch ;

        } /**  END FOR-LOOP COPYING  lname  TO  buffer[], ETC.  **/

        *target = '\0' ; 


        /** COPY VALUE FROM ENVIRONMENT INTO CONTENTS OF eqname **/

    target = eqname ;
    bound  = target + elen ;
    if ( source = getenv( buffer ) )
        {
        for (   ; *target = *source ; target++ , source++ )
            {
            if ( target >= bound ) break ;
            }   /** END FOR-LOOP copying value from environment to eqname **/

        } /**  END IF-CLAUSE:  getenv() succeeded**/
    else
        {
        source = lname ;
        limit  = target + llen ;
        limit  = ( limit < bound ? limit : bound ) ;
        for (  ; target < limit ; *target++ = *source++ )  ; /** EMPTY BODY **/

        } /**  END END ELSE-CLAUSE:  getenv() failed **/

        /** PAD FORTRAN OUTPUT STRING WITH TRAILING BLANKS:  **/

    for (   ; target < bound ; target++ )  *target = ' ' ;

    } /**  END void function nameval() **/


                /** END  CASE:  WIN32 NAMEVAL(): **/
                /** NEXT CASE:  CRAY CF77-TARGETED NAMEVAL(): **/

#elif  defined(_CRAY)


#include <fortran.h>

void NAMEVAL ( _fcd lname, _fcd eqname )
    {
    char buffer[ BUFLEN ] ;
    char *source, *target , *bound, *limit, ch ;
    int length ;

        /** COPY  lname  INTO  buffer[]  AS A C-STRING **/
        /** TRIM TRAILING BLANKS, AND ADD TERMINAL NULL **/
        /** FOR VMS, UPCASE AS YOU GO **/

    source = _fcdtocp( lname ) ;
    length = _fcdlen( lname ) ;
    target = buffer ;
    bound  = target + ( length < BUFLEN-1 ? length : BUFLEN-1 ) ;
    for ( ; target < bound ; target++ , source++ )
        {
        ch = *source ;
        if ( ch == ' ' ) 
            {
            break ;
            } /**  END IF-CLAUSE:  BLANK CHARACTER **/
        else *target = ch ;

        } /**  END FOR-LOOP COPYING  lname  TO  buffer[], ETC.  **/

        *target = '\0' ; 


        /** COPY VALUE FROM ENVIRONMENT INTO CONTENTS OF eqname **/

    target = _fcdtocp( eqname ) ;
    bound  = target + _fcdlen( eqname ) ;
    if ( source = getenv( buffer ) )
        {
        for (   ; *target = *source ; target++ , source++ )
            {
            if ( target >= bound ) break ;
            }   /** END FOR-LOOP copying value from environment to eqname **/

        } /**  END IF-CLAUSE:  getenv() succeeded**/
    else
        {
        source = _fcdtocp(  lname ) ;
        limit  = target + ( _fcdlen( lname ) ) ;
        limit  = ( limit < bound ? limit : bound ) ;
        for (  ; target < limit ; *target++ = *source++ )  ; /** EMPTY BODY **/

        } /**  END END ELSE-CLAUSE:  getenv() failed **/

        /** PAD FORTRAN OUTPUT STRING WITH TRAILING BLANKS:  **/

    for (   ; target < bound ; target++ )  *target = ' ' ;

    } /**  END void function nameval() **/


                /** END CASE OF NAMEVAL() FOR CRAY             **/
                /** REMAINING CASE:  UNSUPPORTED ARCHITECTURES **/
                /** ABORT THE COMPILATION FOR THEM             **/

#else

#error   "Error compiling NAMEVAL():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

