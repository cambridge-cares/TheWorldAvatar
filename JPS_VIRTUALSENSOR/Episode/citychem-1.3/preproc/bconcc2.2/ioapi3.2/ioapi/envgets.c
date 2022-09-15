/**********************************************************************
VERSION "$Id: envgets.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

 LOGICAL     ENVYN()   and  int    envync()
 INTEGER     ENVINT()  and  int    envintc()
 REAL        ENVREAL() and  float  envrealc()
 DOUBLE      ENVDBLE() and  double envdblec()
 SUBROUTINE  ENVSTR()  and  void   envstrc()
 LOGICAL     SETENVVAR()
    
 INTEGER     BENVINT()
 REAL        BENVREAL()
 DOUBLE      BENVDBLE()

      ---->  MACHINE-DEPENDENT !! <----

  find the value of shell variable lname in the environment,
  convert it to the indicated type, and return it

  PRECONDITIONS:  len(lname), len(description) < BUFLEN = 256.
                  case sensitivity handled correctly

  C                     bindings start at line   69:
        envync()    at line   94
        envintc()   at line  171
        envrealc()  at line  227
        envdblec()  at line  283
        envstrc()   at line  340
  Feldman-style Fortran bindings start at line  401
        ENVYN()     at line  547
        ENVINT()    at line  570
        ENVREAL()   at line  629
        ENVDBLE()   at line  691
        ENVSTR()    at line  753
        SETENVVAR() at line  785
        
  WIN32-style   Fortran bindings start at line  689
        ENVYN()     at line  916
        ENVINT()    at line  938
        ENVREAL()   at line  995
        ENVDBLE()   at line 1052
        ENVSTR()    at line 1110
        SETENVVAR() at line 1138

  Cray-style    Fortran bindings start at line  936
        ENVYN()     at line 1270
        ENVINT()    at line 1295
        ENVREAL()   at line 1350
        ENVDBLE()   at line 1408
        ENVSTR()    at line 1485
        SETENVVAR() at line 1493

REVISION HISTORY:
    Prototype 3/1995 by CJC
    Version   8/1999 by CJC for I/O APIv2:  WIN32 stuff
    Version   2/2002 by CJC:  expand BUFLEN to 512; new SETENVVAR()
    Version   2/2002 by CJC:  bugfix for SETENVVAR
    Version   1/2003 by CJC:  copyright statement, improved comments.
    Version   2/2003 by CJC:  Bug-fixes from David Wong, US EPA,
    to Cray versions of lentrim() and SETENVVAR()
    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications, BINFIL3 input
    Format bug-fix 1/2005 by CJC following suggestion by Dr. Michael Bane,
    Univ. Manchester, UK
    Version   4/2005 by CJC:  Bug-fix in envync() from David Wong, US EPA,
    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
    Modified 11/2013 by CJC:  Expand BUFLEN to 64K
    Version  01.2015 by CJC:  BENVINT, BENVREAL, BENVDBLE: additional
    "lo" ahd "hi" arguments
**********************************************************************/

#include "iodecl3.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
                  
#define  BUFLEN  65536


/** ----------------------------------------------------------------- **/
/** ---------------------- C  bindings ------------------------------ **/
/** ----------------------------------------------------------------- **/
/** -----------------------------  envync()  ------------------------ **/

int envync( const char * lname       , 
            const char * description , 
            int          defaultval ,
            int        * status )
    {
    char  *evalue ;
    char   value ;
    char   mesg[ BUFLEN ] ;
    
    if ( evalue = getenv( lname ) )
        {
        if ( value = *evalue )
            {
            if ( value == '.' ) 
                {
                value = evalue[1 ] ;
                }
            if( islower( value ) ) 
                {
                value = toupper( value ) ;
                }

            if ( value == 'Y' || value == 'T' )
                {
                sprintf( mesg,
                         "%s %s:  %s %s",                            
                         "Value for", lname, evalue, "returning TRUE" ) ;
                m3mesgc( mesg ) ;
                *status = 0 ;
                return 1 ;
                }
            else if ( value == 'N' || value == 'F' )
                {
                sprintf( mesg,
                         "%s %s:  %s %s",                            
                         "Value for", lname, evalue, "returning FALSE" ) ;
                m3mesgc( mesg ) ;
                *status = 0 ;
                return 0 ;
                }
            else{
                sprintf( mesg, 
                         "%s %s %s: '%.16s', %s  %s",
                         "Value for",  lname, "not T,F,Y, or N " , evalue, 
                         "returning defaultval  ",
                         defaultval ? "TRUE" : "FALSE" ) ;
                m3errc( "envync", 0, 0, mesg, 0 ) ;
                *status = 1 ;
                return defaultval ;
                }                       /** END:  strtol() failure **/
            }                           /** END:  lname defined      **/
        else{                           /** ELSE  lname defined but empty  **/
            sprintf( mesg,
                     "%s %s %s:  %s",                            
                     "Value for", lname, 
                     "defined but empty; returning default ",
                     defaultval ? "TRUE" : "FALSE" ) ;
            m3mesgc( mesg ) ;
            *status = -1 ;
            return defaultval  ;
            }                           /** END:  lname defined but empty **/
        }                       /** END:  lname defined     **/
    else{                       /** ELSE  lname not defined **/
        sprintf( mesg,
                 "%s %s %s  %s",                            
                 "Value for", lname, 
                 "not defined;returning default: ", 
                 defaultval ? "TRUE" : "FALSE" ) ;
        m3mesgc( mesg ) ;
        *status = -2 ;
        return defaultval ;
        }                       /** END:  lname defined or not **/
    }   /** end int envync() **/


/** -----------------------  envintc()  -------------------------- **/

int envintc( const char *lname,
             const char *description,
             int         defaultval ,
             int        *status )
    {
    char  *evalue, *end ;
    int    value ;
    char   mesg[ BUFLEN ] ;
    
    if ( evalue = getenv( lname ) )
        {
        if ( *evalue )                  /** lname defined **/
            {
            value = (int)strtol( evalue, &end, 10 ) ;
            if ( evalue == end )        /** strtol() failure **/
                {
                sprintf( mesg, 
                         "%s %s %s: '%.16s', %s  %d",
                         "Value for",  lname, "not an integer ", evalue, 
                         "returning default", defaultval ) ;
                m3errc( "envintc", 0, 0, mesg, 0 ) ;
                *status = 1 ;
                return defaultval ;
                }                       /** END:  strtol() failure **/
            else{                       /** ELSE: strtol() success **/
                sprintf( mesg,
                         "%s %s:  %d",                            
                         "Value for", lname, value ) ;
                m3mesgc( mesg ) ;
                *status = 0 ;
                return value ;
                }                       /** END:  strtol() success   **/
            }                           /** END:  lname defined      **/
        else{                           /** ELSE  lname defined but empty  **/
            sprintf( mesg,
                     "%s %s %s:  %d",                            
                     "Value for", lname, 
                     "defined but empty; returning default ", defaultval ) ;
            m3mesgc( mesg ) ;
            *status = -1 ;
            return defaultval  ;
            }                           /** END:  lname defined but empty **/
        }                       /** END:  lname defined     **/
    else{                       /** ELSE  lname not defined **/
        sprintf( mesg,
                 "%s %s %s:  %d",                            
                 "Value for", lname, 
                 "not defined; returning default", defaultval ) ;
        m3mesgc( mesg ) ;
        *status = -2 ;
        return defaultval ;
        }                       /** END:  lname defined or not **/
    }   /** end int envintc() **/


/** ----------------------------  envrealc()  --------------------- **/
float envrealc( const char *lname, 
                const char *description, 
                float       defaultval,
                int        *status )
    {
    char  *evalue, *end ;
    float  value ;
    char   mesg[ BUFLEN ] ;
    
    if ( evalue = getenv( lname ) )
        {
        if ( *evalue )                  /** lname defined **/
            {
            value = (float)strtod( evalue, &end ) ;
            if ( evalue == end )        /** strtol() failure **/
                {
                sprintf( mesg, 
                         "%s %s %s: '%.16s', %s  %G",
                         "Value for",  lname, "not a real ", evalue, 
                         "returning default ", (double) defaultval ) ;
                m3errc( "envrealc", 0, 0, mesg, 0 ) ;
                *status = 1 ;
                return defaultval ;
                }                       /** END:  strtol() failure **/
            else{                       /** ELSE: strtol() success **/
                sprintf( mesg,
                         "%s %s:  %G",                            
                         "Value for", lname, (double) value ) ;
                m3mesgc( mesg ) ;
                *status = 0 ;
                return value ;
                }                       /** END:  strtol() success   **/
            }                           /** END:  lname defined      **/
        else{                           /** ELSE  lname defined but empty  **/
            sprintf( mesg,
                     "%s %s %s:  %G",                            
                     "Value for", lname, 
                     "defined but empty; returning default ", (double) defaultval ) ;
            m3mesgc( mesg ) ;
            *status = -1 ;
            return defaultval  ;
            }                           /** END:  lname defined but empty **/
        }                       /** END:  lname defined     **/
    else{                       /** ELSE  lname not defined **/
        sprintf( mesg,
                 "%s %s %s:  %G",                            
                 "Value for", lname, 
                 "not defined; returning default ", (double) defaultval ) ;
        m3mesgc( mesg ) ;
        *status = -2 ;
        return defaultval ;
        }                       /** END:  lname defined or not **/
    }   /** end int envrealc() **/


/** -------------------------------  envdblec()  --------------------- **/
double envdblec( const char  *lname, 
                 const char  *description, 
                 double       defaultval ,
                 int         *status)
    {
    char   *evalue, *end ;
    double  value ;
    char    mesg[ BUFLEN ] ;
    
    if ( evalue = getenv( lname ) )
        {
        if ( *evalue )                  /** lname defined **/
            {
            value = strtod( evalue, &end ) ;
            if ( evalue == end )        /** strtod() failure **/
                {
                sprintf( mesg, 
                         "%s %s %s: '%.16s', %s  %G",
                         "Value for",  lname, "not a double ", evalue, 
                         "returning default:", defaultval ) ;
                m3errc( "envdblec", 0, 0, mesg, 0 ) ;
                *status = 1 ;
                return defaultval ;
                }                       /** END:  strtod() failure **/
            else{                       /** ELSE: strtod() success **/
                sprintf( mesg,
                         "%s %s:  %G",                            
                         "Value for ", lname, value ) ;
                m3mesgc( mesg ) ;
                *status = 0 ;
                return value ;
                }                       /** END:  strtod() success   **/
            }                           /** END:  lname defined      **/
        else{                           /** ELSE  lname defined but empty  **/
            sprintf( mesg,
                     "%s %s %s:  %G",                            
                     "Value for", lname, 
                     "defined but empty; returning default ", defaultval ) ;
            m3mesgc( mesg ) ;
            *status = -1 ;
            return defaultval  ;
            }                           /** END:  lname defined but empty **/
        }                       /** END:  lname defined     **/
    else{                       /** ELSE  lname not defined **/
        sprintf( mesg,
                 "%s %s %s:  %G",                            
                 "Value for", lname, 
                 "not defined;returning default ", defaultval ) ;
        m3mesgc( mesg ) ; 
        *status = -2 ;
        return defaultval ;
        }                       /** END:  lname defined or not **/
    }   /** end int envdblec() **/


/** -------------------------------------------------------------- **/

void envstrc( const char * lname, 
              const char * description, 
              const char * defaultval,
              char       * evalue,
              int        * status,
              int          elen )
    {
    char   mesg[ BUFLEN ] ;
    char  *value ;
    size_t length ;
    
    if ( value = getenv( lname ) )
        {
        if ( *value )                  /** lname defined **/
            {
            strncpy( evalue, value, elen ) ;
            sprintf( mesg,
                     "%s %s:  '",                            
                     "Value for", lname ) ;
            length = BUFLEN - STRLEN( mesg ) - 2 ;
            length = ( length < elen ? length : elen ) ;
            strncat( mesg, evalue, length ) ;
            strncat( mesg, "'", (size_t)1 ) ;
            m3mesgc( mesg ) ; 
            *status = 0 ;
            }
        else{
            strncpy( evalue, defaultval, elen ) ;
            sprintf( mesg,
                     "%s %s %s:  '",                            
                     "Value for", lname, 
                     "defined but empty; returning default" ) ;
            length = BUFLEN - STRLEN( mesg ) - 2 ;
            length = ( length < elen ? length : elen ) ;
            strncat( mesg, evalue, length ) ;
            strncat( mesg, "'", (size_t)1 ) ;
            m3mesgc( mesg ) ; 
            *status = -1 ;
            }                           /** END:  lname defined but empty **/
        }                       /** END:  lname defined     **/
    else{                       /** ELSE  lname not defined **/
        strncpy( evalue, defaultval, elen ) ;
        sprintf( mesg,
                 "%s %s %s:  '",                            
                 "Value for", lname, 
                 "not defined; returning defaultval '" ) ;
        length = BUFLEN - STRLEN( mesg ) - 2 ;
        length = ( length < elen ? length : elen ) ;
        strncat( mesg, evalue, length ) ;
        strncat( mesg, "'", (size_t)1 ) ;
            m3mesgc( mesg ) ; 
        *status = -2 ;
        }                       /** END:  lname defined or not **/

    m3mesgc( mesg ) ;
    return ;

    }   /** end int envstrc() **/


/** -------------------------------------------------------------- **/
/** ---------------------- Fortran bindings ---------------------- **/
/** -------------------------------------------------------------- **/
/** CODE DEPENDS (BADLY) UPON HOW THE FORTRAN COMPILER DEALS WITH  **/
/** NAMES AND WITH CHARACTER STRINGS:                              **/
/** 3-CASES HERE:  FELDMAN-DESCENDED F77'S, WIN32 F77'S,           **/
/** AND CRAY CF77'S TARGETED AS CALLERS OF ENV*().                 **/
/** -------------------------------------------------------------- **/
/** FIRST CASE:  FELDMANISMS:                                    **/


#if FLDMN

/** Hack for Feldman-descended f77's follows: **/

#define ENVYN      envyn_
#define ENVINT     envint_
#define ENVREAL    envreal_
#define ENVDBLE    envdble_
#define ENVSTR     envstr_
#define SETENVVAR  setenvvar_

#define BENVINT    benvint_
#define BENVREAL   benvreal_
#define BENVDBLE   benvdble_

#elif defined(__hpux) || defined(_AIX)

/** Hack for no-underscore quasi-Feldman-descended f77's follows: **/

#define ENVYN      envyn
#define ENVINT     envint
#define ENVREAL    envreal
#define ENVDBLE    envdble
#define ENVSTR     envstr
#define SETENVVAR  setenvvar

#define BENVINT    benvint
#define BENVREAL   benvreal
#define BENVDBLE   benvdble

#endif

#if FLDMN || defined(__hpux) || defined(_AIX) || defined(ABSFT)


/** -------------------------------------------------------------- **/
/** ------------------------  auxilliary functions  -------------- **/
/** -------------------------------------------------------------- **/
/** ----------------------  name2cstr()  ------------------------- **/

void  name2cstr( const char * source, 
                       char       * target,
                       FSTR_L       slen,
                       FSTR_L       tlen )
    {
    char  *bound ;
    char   ch ;
    int    length ;
    
    tlen-- ;
    target[ tlen ] = '\0' ;
    length = ( slen < tlen-1 ? slen : tlen-1 ) ;
    bound  = target + length ;

    for ( ; target < bound ; target++ , source++ )
        {
        ch = *source ;
        if ( isspace( ch ) )
            {
            *target = '\0' ;
            return ;
            }
        *target = ch ;

        } /**  END FOR-LOOP COPYING  lname  TO  buffer[], ETC.  **/
    
    *target = '\0' ;
    
    }    /** END Feldmanish name2cstr() **/
    

/** -------------------- fstr2cstr() ----------------------------- **/

void  fstr2cstr( const char * source, 
                        char       * target, 
                        FSTR_L       slen, 
                        FSTR_L       tlen )
    {
    char  *bound ;
    FSTR_L length ;

    tlen-- ;

    for ( length = ( slen < tlen-1 ? slen : tlen ) ,
          bound  = target + length ;
              target < bound ;
                  target++ , source++ )
        {
        *target = *source ;
        }
    
    *target = '\0' ;
    
    }       /** END Feldmanish fstr2cstr() **/


/** --------------------- cstr2fstr() ---------------------------- **/

void  cstr2fstr( const char * source, 
                        char *       target, 
                        FSTR_L       tlen )
    {
    char *bound, ch ;

    for ( bound  = target + tlen ; target < bound ; target++ , source++ )
        {
        if ( ! ( ch = *source ) ) break ;
        *target = ch ;
        }
    for ( ; target < bound ; target++ )
        {
        *target = ' ' ;
        }
    }         /** END Feldmanish cstr2fstr() **/


/** --------------------- cstr2fstr() ---------------------------- **/

static int  lentrim( const char * source, 
                     const FSTR_L slen )
    {
    const char * ch ;
    int    len ;

    ch  = & source[ slen - 1 ];
    len = slen ;
    for ( ; len >= 0 ; ch--, len-- )
        {
        if ( *ch != ' ' ) break ;
        }
    return( len ) ;
    }         /** END Feldmanish cstr2fstr() **/


/** --------------------------  ENVYN()  ------------------------- **/

FINT ENVYN( const char * lname, 
            const char * descrip, 
            const FINT * defaultval,
            FINT       * status,
            FSTR_L       llen, 
            FSTR_L       dlen )
    {
    char  nbuff[ BUFLEN ] ;
    char  dbuff[ BUFLEN ] ;
    int   result, istat ;

    name2cstr( lname,   nbuff, llen, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, dlen, BUFLEN ) ;

    result  = envync( nbuff, dbuff, (int)*defaultval, & istat ) ;
    *status = (FINT) istat ;
    return    (FINT) result ;

    } /**  END Feldmanish int function ENVYN() **/


/** ------------------------  ENVINT()  and BENVINT()  -------------------------- **/

FINT ENVINT( const char * lname, 
             const char * descrip, 
             const FINT * defaultval,
             FINT       * status,
             FSTR_L       llen, 
             FSTR_L       dlen )
    {
    char  nbuff[ BUFLEN ] ;
    char  dbuff[ BUFLEN ] ;
    int   result, istat ;

    name2cstr( lname,   nbuff, llen, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, dlen, BUFLEN ) ;

    result  = (FINT) envintc( nbuff, dbuff, (int)*defaultval, & istat ) ;
    *status = (FINT) istat ;
    return    (FINT) result ;

    } /**  END Feldmanish int function ENVINT() **/


FINT BENVINT( const char * lname, 
              const char * descrip, 
              const FINT * loval,
              const FINT * hival,
              const FINT * defaultval,
              FINT       * status,
              FSTR_L       llen, 
              FSTR_L       dlen )
    {
    char  nbuff[ BUFLEN ] ;
    char  dbuff[ BUFLEN ] ;
    int   result, istat ;

    name2cstr( lname,   nbuff, llen, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, dlen, BUFLEN ) ;

    result  = (FINT) envintc( nbuff, dbuff, (int)*defaultval, & istat ) ;

    if ( istat )
        {
        *status = (FINT) istat ;
        }
    else if ( ( result < * loval ) || ( result > * hival ) )
        {
        * status = 2 ;
        result   = * defaultval ;
        }
    else{
        * status = 0 ;
        }

    return    (FINT) result ;

    } /**  END Feldmanish int function ENVINT() **/


/** ------------------------  ENVREAL() and BENVREAL()  ------------------------- **/

FREAL ENVREAL( const char  * lname, 
               const char  * descrip, 
               const FREAL * defaultval,
               FINT        * status,
               FSTR_L        llen, 
               FSTR_L        dlen )
    {
    char  nbuff[ BUFLEN ] ;
    char  dbuff[ BUFLEN ] ;
    float result ;
    int   istat ;

    name2cstr( lname, nbuff, llen, BUFLEN ) ;
    fstr2cstr( lname, dbuff, dlen, BUFLEN ) ;

    result  = envrealc( nbuff, dbuff, (float)* defaultval, & istat ) ;
    *status = (FINT) istat ;

    return (FREAL) result ;

    } /**  END Feldmanish float function ENVREAL() **/


FREAL BENVREAL( const char  * lname, 
                const char  * descrip, 
                const FREAL * loval,
                const FREAL * hival,
                const FREAL * defaultval,
                FINT        * status,
                FSTR_L        llen, 
                FSTR_L        dlen )
    {
    char  nbuff[ BUFLEN ] ;
    char  dbuff[ BUFLEN ] ;
    float result ;
    int   istat ;

    name2cstr( lname, nbuff, llen, BUFLEN ) ;
    fstr2cstr( lname, dbuff, dlen, BUFLEN ) ;

    result  = envrealc( nbuff, dbuff, (float)* defaultval, & istat ) ;

    if ( istat )
        {
        *status = (FINT) istat ;
        }
    else if ( ( result < * loval ) || ( result > * hival ) )
        {
        * status = 2 ;
        result   = * defaultval ;
        }
    else{
        * status = 0 ;
        }

    return (FREAL) result ;

    } /**  END Feldmanish float function ENVREAL() **/


/** -------------------------  ENVDBLE()  ------------------------ **/

double ENVDBLE( const char   * lname, 
                const char   * descrip, 
                const double * defaultval,
                int          * status,
                FSTR_L         llen, 
                FSTR_L         dlen )
    {
    char   nbuff[ BUFLEN ] ;
    char   dbuff[ BUFLEN ] ;
    int    istat ;
    double result ;

    name2cstr( lname,   nbuff, llen, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, dlen, BUFLEN ) ;

    result  = envdblec( nbuff, dbuff, *defaultval, & istat ) ;
    *status = istat ;

    return result ;

    } /**  END Feldmanish double function ENVDBLE() **/


double BENVDBLE( const char   * lname, 
                 const char   * descrip, 
                 const double * loval,
                 const double * hival,
                 const double * defaultval,
                 int          * status,
                 FSTR_L         llen, 
                 FSTR_L         dlen )
    {
    char   nbuff[ BUFLEN ] ;
    char   dbuff[ BUFLEN ] ;
    int    istat ;
    double result ;

    name2cstr( lname,   nbuff, llen, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, dlen, BUFLEN ) ;

    result  = envdblec( nbuff, dbuff, *defaultval, & istat ) ;

    if ( istat )
        {
        *status = (FINT) istat ;
        }
    else if ( ( result < * loval ) || ( result > * hival ) )
        {
        * status = 2 ;
        result   = * defaultval ;
        }
    else{
        * status = 0 ;
        }

    return result ;

    } /**  END Feldmanish double function ENVDBLE() **/


/** ------------------------  ENVSTR()  -------------------------- **/

void ENVSTR( const char * lname, 
             const char * description, 
             const char * defaultval, 
             char       * eqname, 
             FINT       * status,
             FSTR_L       namlen, 
             FSTR_L       deslen, 
             FSTR_L       deflen, 
             FSTR_L       eqlen )
    {
    char nambuf[ BUFLEN ] ;
    char desbuf[ BUFLEN ] ;
    char defbuf[ BUFLEN ] ;
    char eqbuf [ BUFLEN ] ;
    int  istat ;

    name2cstr( lname      , nambuf, namlen, BUFLEN ) ;
    fstr2cstr( description, desbuf, deslen, BUFLEN ) ;
    fstr2cstr( defaultval , defbuf, deflen, BUFLEN ) ;
    
    envstrc( nambuf, desbuf, defbuf, eqbuf, & istat, eqlen ) ;
    
    cstr2fstr( eqbuf, eqname, eqlen ) ;
    *status = istat ;
    
    return ;

    } /**  END Feldmanish void function ENVSTR() **/


/** ------------------------  SETENVVAR()  -------------------------- **/

FINT SETENVVAR( const char * lname, 
                const char * value, 
                FSTR_L       namlen, 
                FSTR_L       vallen )
    {
    char * buffer;   /*  construct "name=value" here */
    FSTR_L l1, l2 ;

    l1 = lentrim( lname, namlen ) ;
    l2 = lentrim( value, vallen ) ;
    buffer = malloc( l1 + l2 + 3 ) ;
    if ( ! buffer ) 
        {
        m3mesgc( "SETENVVAR:  malloc() failure" ) ;
        return( 0 ) ;
        }

    memcpy( buffer, lname, l1 ) ;
    buffer[ l1 ] = '=' ;
    memcpy( buffer+l1+1, value, l2 ) ;
    buffer[ l1+l2+1 ] = '\0' ;
    
    return (FINT) ( 0 == putenv( buffer ) );

    } /**  END Feldmanish void function SETENVVAR() **/


/** -------------------------------------------------------------- **/
/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS FOR USE             **/
/** NEXT CASE:  WIN32-TARGETED ENV*():                         **/
/** -------------------------------------------------------------- **/

#elif  defined(_WIN32)

/** -------------------------------------------------------------- **/
/** ------------------------  auxilliary functions  -------------- **/
/** -------------------------------------------------------------- **/
/** ----------------------  name2cstr()  ------------------------- **/

void  name2cstr( const char * source, 
                        char       * target,
                        FSTR_L       slen,
                        FSTR_L       tlen )
    {
    char  *bound ;
    char   ch ;
    int    length ;
    
    tlen-- ;

    for ( length = ( slen < tlen ? slen : tlen ) ,
          bound  = target + length ;
              target < bound ;
                  target++ , source++ )
        {
        ch = *source ;
        if ( isspace( ch ) ) break ;
        *target = ch ;

        } /**  END FOR-LOOP COPYING  lname  TO  buffer[], ETC.  **/
    
    *target = '\0' ;
    
    }    /** END WIN32 name2cstr() **/
    

/** -------------------- fstr2cstr() ----------------------------- **/

void  fstr2cstr( const char * source, 
                        char       * target, 
                        FSTR_L       slen, 
                        FSTR_L       tlen )
    {
    char *bound, ch ;
    int length ;

    tlen-- ;

    for ( length = ( slen < tlen ? slen : tlen ) ,
          bound  = target + length ;
              target < bound ;
                  target++ , source++ )
        {
        *target = *source ;
        }
    
    *target = '\0' ;
    
    }       /** END WIN32 fstr2cstr() **/


/** --------------------- cstr2fstr() ---------------------------- **/

void  cstr2fstr( const char * source, 
                        char *       target, 
                        FSTR_L       tlen )
    {
    char *bound, ch ;

    for ( bound  = target + tlen ; target < bound ; target++ , source++ )
        {
        if ( ! ( ch = *source ) ) break ;
        *target = ch ;
        }
    for ( ; target < bound ; target++ )
        {
        *target = ' ' ;
        }
    }         /** END WIN32 cstr2fstr() **/


/** --------------------- lentrim() ---------------------------- **/

static int  lentrim( const char *  source, 
                     const FSTR_L  slen )
    {
    char * ch ;
    int    len ;

    ch  = source + slen - 1 ;
    len = slen ;
    for ( ; len >= 0 ; ch--, len-- )
        {
        if ( *ch != ' ' ) break
        }
    return( len ) ;
    }         /** END WIN32 lentrim() **/


/** --------------------------  ENVYN()  ------------------------- **/

FINT ENVYN( const char * lname,   FSTR_L  llen, 
            const char * descrip, FSTR_L  dlen,
            const FINT * defaultval,
            FINT       * status)
    {
    char nbuff[ BUFLEN ] ;
    char dbuff[ BUFLEN ] ;
    int  istat, result ;

    name2cstr( lname,   nbuff, llen, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, dlen, BUFLEN ) ;

    result  = envync( nbuff, dbuff, (int)*defaultval, & istat ) ;
    *status = (FINT) istat ;

    return ( (FINT) result ) ;

    } /**  END WIN32 int function ENVYN() **/


/** ------------------------  ENVINT()  -------------------------- **/

FINT ENVINT( const char * lname,   FSTR_L  llen, 
             const char * descrip, FSTR_L  dlen,
             const FINT * defaultval,
             FINT       * status)
    {
    char nbuff[ BUFLEN ] ;
    char dbuff[ BUFLEN ] ;
    int  istat, result ;

    name2cstr( lname,   nbuff, llen, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, dlen, BUFLEN ) ;

    result  = envintc( nbuff, dbuff, (int)*defaultval, & istat ) ;

    *status = (FINT) istat ;

    return ( (FINT) result );

    } /**  END WIN32 int function ENVINT() **/


FINT BENVINT( const char * lname,   FSTR_L  llen, 
              const char * descrip, FSTR_L  dlen,
              const FINT * loval,
              const FINT * hival,
              const FINT * defaultval,
              FINT       * status)
    {
    char nbuff[ BUFLEN ] ;
    char dbuff[ BUFLEN ] ;
    int  istat, result ;

    name2cstr( lname,   nbuff, llen, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, dlen, BUFLEN ) ;

    result  = envintc( nbuff, dbuff, (int)*defaultval, & istat ) ;

    if ( istat )
        {
        *status = (FINT) istat ;
        }
    else if ( ( result < * loval ) || ( result > * hival ) )
        {
        * status = 2 ;
        result   = * defaultval ;
        }
    else{
        * status = 0 ;
        }

    return ( (FINT) result );

    } /**  END WIN32 int function ENVINT() **/


/** ------------------------  ENVREAL()  ------------------------- **/

float ENVREAL( const char * lname,   FSTR_L  llen, 
               const char * descrip, FSTR_L  dlen,
               const FREAL * defaultval,
               FINT        * status)
    {
    char  nbuff[ BUFLEN ] ;
    char  dbuff[ BUFLEN ] ;
    int   istat ;
    float result ;

    name2cstr( lname, nbuff, llen, BUFLEN ) ;
    fstr2cstr( lname, dbuff, dlen, BUFLEN ) ;

    result  = envrealc( nbuff, dbuff, (float)*defaultval, & istat ) ;
    *status = (FINT)  istat ;
    return   (FREAL) result ;

    } /**  END WIN32 float function ENVREAL() **/


float BENVREAL( const char * lname,   FSTR_L  llen, 
                const char * descrip, FSTR_L  dlen,
                const FREAL * loval,
                const FREAL * hival,
                const FREAL * defaultval,
                FINT        * status)
    {
    char  nbuff[ BUFLEN ] ;
    char  dbuff[ BUFLEN ] ;
    int   istat ;
    float result ;

    name2cstr( lname, nbuff, llen, BUFLEN ) ;
    fstr2cstr( lname, dbuff, dlen, BUFLEN ) ;

    result  = envrealc( nbuff, dbuff, (float)*defaultval, & istat ) ;

    if ( istat )
        {
        *status = (FINT) istat ;
        }
    else if ( ( result < * loval ) || ( result > * hival ) )
        {
        * status = 2 ;
        result   = * defaultval ;
        }
    else{
        * status = 0 ;
        }
        
    return   (FREAL) result ;

    } /**  END WIN32 float function ENVREAL() **/


/** -------------------------  ENVDBLE()  ------------------------ **/

double ENVDBLE( const char * lname,   FSTR_L  llen, 
                const char * descrip, FSTR_L  dlen,
                const double * defaultval,
                FINT         * status)
    {
    int    istat ;
    double result ;
    char   nbuff[ BUFLEN ] ;
    char   dbuff[ BUFLEN ] ;

    name2cstr( lname,   nbuff, llen, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, dlen, BUFLEN ) ;

    result  = envdblec( nbuff, dbuff, *defaultval, & istat ) ;
    *status = (FINT) istat ;

    return  result ;

    } /**  END WIN32 double function ENVDBLE() **/


double BENVDBLE( const char * lname,   FSTR_L  llen, 
                 const char * descrip, FSTR_L  dlen,
                 const double * loval,
                 const double * hival,
                 const double * defaultval,
                 FINT         * status)
    {
    int    istat ;
    double result ;
    char   nbuff[ BUFLEN ] ;
    char   dbuff[ BUFLEN ] ;

    name2cstr( lname,   nbuff, llen, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, dlen, BUFLEN ) ;

    result  = envdblec( nbuff, dbuff, *defaultval, & istat ) ;

    if ( istat )
        {
        *status = (FINT) istat ;
        }
    else if ( ( result < * loval ) || ( result > * hival ) )
        {
        * status = 2 ;
        result   = * defaultval ;
        }
    else{
        * status = 0 ;
        }

    return  result ;

    } /**  END WIN32 double function ENVDBLE() **/


/** ------------------------  ENVSTR()  -------------------------- **/

void ENVSTR( const char * lname,      FSTR_L  namlen, 
             const char * descrip,    FSTR_L  deslen, 
             const char * defaultval, FSTR_L  deflen, 
             char       * eqname,     FSTR_L  eqlen,
             FINT       * status)
    {
    char nambuf[ BUFLEN ] ;
    char desbuf[ BUFLEN ] ;
    char defbuf[ BUFLEN ] ;
    char eqbuf [ BUFLEN ] ;
    int  istat ;

    name2cstr( lname      , nambuf, namlen, BUFLEN ) ;
    fstr2cstr( descrip    , desbuf, deslen, BUFLEN ) ;
    fstr2cstr( defaultval , defbuf, deflen, BUFLEN ) ;
    
    envstrc( nambuf, desbuf, defbuf, eqbuf, & istat, eqlen ) ;
    
    cstr2fstr( eqbuf, eqname, eqlen ) ;
    *status = (FINT) istat ;
    
    return ;

    } /**  END WIN32 void function ENVSTR() **/


/** ------------------------  SETENVVAR()  -------------------------- **/

FINT SETENVVAR( const char * lname, FSTR_L  namlen, 
                const char * value, FSTR_L  vallen )
    {
    char * buffer;   /*  construct "name=value" here */
    int l1, l2 ;

    l1 = lentrim( lname, namlen ) ;
    l2 = lentrim( value, vallen ) ;
    buffer = malloc( l1 + l2 + 3 ) ;
    if ( ! buffer ) 
        {
        m3mesgc( "SETENVVAR:  malloc() failure" ) ;
        return( 0 ) ;
        }

    memcpy( buffer, lname, l1 ) ;
    buffer[ l1 ] = '=' ;
    memcpy( buffer+l1+1, value, l2 ) ;
    buffer[ l1+l2+1 ] = '\0' ;
    
    return (FINT)( 0 == putenv( buffer ) );

    } /**  END WIN32 void function SETENVVAR() **/


/** -------------------------------------------------------------- **/
/** END  CASE OF WIN32 TARGETS FOR USE                             **/
/** NEXT CASE:  CRAY CF77-TARGETED ENV*():                         **/
/** -------------------------------------------------------------- **/

#elif  defined(_CRAY)

#include <fortran.h>

/** -------------------------------------------------------------- **/
/** ------------------------  auxilliary functions  -------------- **/
/** -------------------------------------------------------------- **/

void  name2cstr( const _fcd   source, 
                        char       * target,
                        FSTR_L       tlen  )
    {
    char  *bound, *ptr ;
    char   ch ;
    int    slen, length ;
    
    slen = _fcdlen( source ) ;
    tlen-- ;

    length = ( slen < tlen ? slen : tlen ) ;
    ptr    = _fcdtocp( source ) ;
    bound  = ptr + length ;
    
    for ( ; ptr < bound ; target++ , ptr++ )
        {
        if ( isspace( ch = *ptr ) ) break ;
        *target = ch ;

        } /**  END FOR-LOOP COPYING  lname  TO  buffer[], ETC.  **/

    *target = '\0' ;

    }           /** END Cray name2cstr() **/
    

/** -------------------- fstr2cstr() ----------------------------- **/

void  fstr2cstr( const _fcd   source, 
                        char       * target, 
                        FSTR_L       tlen )
    {
    char *ptr, *bound, ch ;
    int   slen, length ;

    slen = _fcdlen( source ) ;
    tlen-- ;

    length = ( slen < tlen ? slen : tlen ) ;
    ptr    = _fcdtocp( source ) ;
    for ( bound  = ptr + length ; ptr < bound ; target++ , ptr++ )
        {
        *target = *ptr ;
        }
    
    *target = '\0' ;
    
    }           /** END Cray fstr2cstr() **/


/** --------------------- cstr2fstr() ---------------------------- **/

void  cstr2fstr( const char * source,
                        _fcd         target )
    {
    char *bound ;
    char *ptr ;
    char  ch ;
    int   length ;

    length = _fcdlen ( target ) ;
    ptr    = _fcdtocp( target ) ;
    for (  bound  = ptr + length ;  ptr < bound ;  ptr++ , source++ )
        {
        if ( !( ch = *source ) ) break ;
        *ptr = ch ;
        }
    for ( ; ptr < bound ; ptr++ )
        {
        *ptr = ' ' ;
        }
    }           /** END Cray cstr2fstr() **/


/** --------------------- cstr2fstr() ---------------------------- **/

static int  lentrim( _fcd source )
    {
    char * ch ;
    int    len ;

    len = _fcdlen ( source ) ;
    ch  = _fcdtocp( source ) + len - 1 ;
    for ( ; len >= 0 ; ch--, len-- )
        {
        if ( *ch != ' ' ) break ;
        } ;
    return( len ) ;
    }         /** END Cray lentrim() **/


/** --------------------------  ENVYN()  ------------------------- **/

FINT ENVYN( const _fcd   lname, 
            const _fcd   descrip, 
            const FINT * defaultval,
            FINT       * status )
    {
    char * getenv();
    int    in, out ;
    int    istat ;

    char nbuff[ BUFLEN ] ;
    char dbuff[ BUFLEN ] ;

    name2cstr( lname,   nbuff, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, BUFLEN ) ;

    in  = _ltob( defaultval ) ;
    out = envync( nbuff, dbuff, in, & istat ) ;
    *status = (FINT) istat ;
    return _btol( out ) ;

    } /**  END Cray int function ENVYN() **/


/** ------------------------  ENVINT()  -------------------------- **/

int ENVINT( const _fcd   lname, 
            const _fcd   descrip, 
            const FINT * defaultval,
            FINT       * status )
    {
    char nbuff[ BUFLEN ] ;
    char dbuff[ BUFLEN ] ;
    int  result, istat ;

    name2cstr( lname  , nbuff, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, BUFLEN ) ;

    result  = envintc( nbuff, dbuff, (int)*defaultval, & istat ) ;
    *status = (FINT) istat ;
    return    (FINT) result ;

    } /**  END Cray int function ENVINT() **/


int BENVINT( const _fcd   lname, 
             const _fcd   descrip, 
             const FINT * loval,
             const FINT * hival,
             const FINT * defaultval,
             FINT       * status )
    {
    char nbuff[ BUFLEN ] ;
    char dbuff[ BUFLEN ] ;
    int  result, istat ;

    name2cstr( lname  , nbuff, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, BUFLEN ) ;

    result  = envintc( nbuff, dbuff, (int)*defaultval, & istat ) ;

    if ( istat )
        {
        *status = (FINT) istat ;
        }
    else if ( ( result < * loval ) || ( result > * hival ) )
        {
        * status = 2 ;
        result   = * defaultval ;
        }
    else{
        * status = 0 ;
        }
        
    return    (FINT) result ;

    } /**  END Cray int function BENVINT() **/


/** ------------------------  ENVREAL()  ------------------------- **/

FREAL ENVREAL( const _fcd    lname, 
               const _fcd    descrip, 
               const FREAL * defaultval,
               FINT        * status )
    {
    char  nbuff[ BUFLEN ] ;
    char  dbuff[ BUFLEN ] ;
    float result ;
    int   istat ;

    name2cstr( lname,   nbuff, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, BUFLEN ) ;

    result  = envrealc( nbuff, dbuff, (float)*defaultval, & istat ) ;
    *status = istat ;

    return (FREAL) result ;

    } /**  END Cray float function ENVREAL() **/


FREAL BENVREAL( const _fcd    lname, 
                const _fcd    descrip, 
                const FREAL * loval,
                const FREAL * hival,
                const FREAL * defaultval,
                FINT        * status )
    {
    char  nbuff[ BUFLEN ] ;
    char  dbuff[ BUFLEN ] ;
    float result ;
    int   istat ;

    name2cstr( lname,   nbuff, BUFLEN ) ;
    fstr2cstr( descrip, dbuff, BUFLEN ) ;

    result  = envrealc( nbuff, dbuff, (float)*defaultval, & istat ) ;

    if ( istat )
        {
        *status = (FINT) istat ;
        }
    else if ( ( result < * loval ) || ( result > * hival ) )
        {
        * status = 2 ;
        result   = * defaultval ;
        }
    else{
        * status = 0 ;
        }

    return (FREAL) result ;

    } /**  END Cray float function BENVREAL() **/


/** -------------------------  ENVDBLE()  ------------------------ **/

double ENVDBLE( const _fcd     lname, 
                const _fcd     descrip, 
                const double * defaultval,
                FINT         * status )
    {
    char   nbuff[ BUFLEN ] ;
    char   dbuff[ BUFLEN ] ;
    int    istat ;
    double result ;

    name2cstr( lname, nbuff, BUFLEN ) ;
    fstr2cstr( lname, dbuff, BUFLEN ) ;

    result  = envdblec( nbuff, dbuff, *defaultval, & istat ) ;
    *status = istat ;

    return result ;

    } /**  END Cray double function ENVDBLE() **/

double BENVDBLE( const _fcd     lname, 
                 const _fcd     descrip, 
                 const double * loval,
                 const double * hival,
                 const double * defaultval,
                 FINT         * status )
    {
    char   nbuff[ BUFLEN ] ;
    char   dbuff[ BUFLEN ] ;
    int    istat ;
    double result ;

    name2cstr( lname, nbuff, BUFLEN ) ;
    fstr2cstr( lname, dbuff, BUFLEN ) ;

    result  = envdblec( nbuff, dbuff, *defaultval, & istat ) ;

    if ( istat )
        {
        *status = (FINT) istat ;
        }
    else if ( ( result < * loval ) || ( result > * hival ) )
        {
        * status = 2 ;
        result   = * defaultval ;
        }
    else{
        * status = 0 ;
        }

    return result ;

    } /**  END Cray double function BENVDBLE() **/


/** ------------------------  ENVSTR()  -------------------------- **/

void ENVSTR( const _fcd   lname, 
             const _fcd   description, 
             const _fcd   defaultval, 
             _fcd         eqname,
             FINT       * status )
    {
    char nambuf[ BUFLEN ] ;
    char desbuf[ BUFLEN ] ;
    char defbuf[ BUFLEN ] ;
    char eqbuf [ BUFLEN ] ;
    int  istat ;

    name2cstr( lname      , nambuf, BUFLEN ) ;
    fstr2cstr( description, desbuf, BUFLEN ) ;
    fstr2cstr( defaultval , defbuf, BUFLEN ) ;
    
    envstrc( nambuf, desbuf, defbuf, eqbuf, & istat, BUFLEN ) ;
    
    cstr2fstr( eqbuf, eqname ) ;
    *status = istat ;

    return ;

    } /**  END Cray void function ENVSTR() **/


/** ------------------------  SETENVVAR()  -------------------------- **/

FINT SETENVVAR( const _fcd   lname, 
                const _fcd   value )
    {
    char * buffer ;   /*  construct "name=value" here */
    int l1, l2 ;
    char nbuff[ BUFLEN ] ;
    char vbuff[ BUFLEN ] ;

    name2cstr( lname, nbuff, BUFLEN ) ;
    fstr2cstr( value, vbuff, BUFLEN ) ;

    l1 = lentrim( lname ) ;
    l2 = lentrim( value ) ;
    buffer = malloc( l1 + l2 + 3 ) ;
    if ( ! buffer ) 
        {
        m3mesgc( "SETENVVAR:  malloc() failure" ) ;
        return _ltob( 0 ) ;
        }

    memcpy( buffer, nbuff, l1 ) ;
    buffer[ l1 ] = '=' ;
    memcpy( buffer+l1+1, vbuff, l2 ) ;
    buffer[ l1+l2+1 ] = '\0' ;

    return _ltob( 0 == putenv( buffer ) );

    } /**  END Crayish void function SETENVVAR() **/


/** -------------------------------------------------------------- **/
/** END CASE OF ENV*() FOR CRAY                                    **/
/** REMAINING CASE:  UNSUPPORTED ARCHITECTURES                     **/
/** ABORT THE COMPILATION FOR THEM                                 **/
/** -------------------------------------------------------------- **/

#else

#error   "Error compiling envgets.c:  unsupported architecture"

#endif   /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF WIN32, OR IF CRAY **/
    



