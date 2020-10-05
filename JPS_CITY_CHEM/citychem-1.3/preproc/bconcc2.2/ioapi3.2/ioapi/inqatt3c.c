
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "inqatt3c.c" version "$Id: inqatt3c.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    returns list of attributes, their types, and sizes for the
    file FNAME and variable VNAME (or ALLVAR3 for "global" file
    attributes).
   Wrapper around I/O API Fortran-binding routine INQATT3()

PRECONDITIONS:
    FNAME already opened by OPEN3() or open3c()
    VNAME a valid variable in FNAME, or else is ALLVAR3=='ALL'

CALLS:
    Fortran I/O API's INQATT3()

REVISION HISTORY:
    Prototype 1/2002 by C

    Cray conditional-compile bug fix 4/2002 by C

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED FC'S FOLLOW: **/

#if FLDMN

#define INQATT3 inqatt3_

#elif defined(__hpux) || defined(_AIX)

#define INQATT3 inqatt3

#endif

#define  BUFLEN  512


#if defined(INQATT3) || defined(ABSFT)

    extern FINT INQATT3( const char * fname ,
                         const char * vname ,
                         const FINT * mxatts,
                         FINT       * natts,
                         char       * aname,
                         FINT         atypes[],
                         FINT         asizes[],
                         FSTR_L       fnamelen ,
                         FSTR_L       vnamelen ,
                         FSTR_L       anamelen ) ;

int inqatt3c( const char          * fname ,
              const char          * vname ,
              int                   maxatt ,
              int                   attnamelen ,
              int                 * numatts ,
              char                * attname,
              int                   atype[],
              int                   asize[] )

    {       /*  begin body of inqatt3c() */
    int  i, result ;
    FINT maxa, numa, atyp[ MXATTS3 ] , asiz[ MXATTS3 ]  ;

    result = (int) INQATT3( fname ,
                            vname ,
                            & maxa ,
                            & numa ,
                            attname ,
                            atyp ,
                            asiz ,
                            STRLEN( fname ) ,
                            STRLEN( vname ) ,
                            (FSTR_L)attnamelen ) ;

    if ( result )
        {
        * numatts = numa ;
        for( i = 0 ; i < numa ; i++ )
            {
            atype[ i ] = (int) atyp[ i ] ;
            asize[ i ] = (int) asiz[ i ] ;
            }
        }

    return result ;

    }       /*  end body of  inqatt3c()  */


                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  Win32 inqatt3c(): **/

#elif defined(_WIN32)

#include <stdio.h> /* for sprintf */

    extern FINT INQATT3( const char * fname ,
                         FSTR_L       fnamelen ,
                         const char * vname ,
                         FSTR_L       vnamelen ,
                         const FINT * mxatts,
                         FINT       * natts,
                         char       * aname,
                         FSTR_L       anamelen ,
                         FINT         atypes[],
                         FINT         asizes[] ) ;

int inqatt3c( const char          * fname ,
              const char          * vname ,
              int                   maxatt ,
              int                   attnamelen ,
              int                 * numatts ,
              char                * attname,
              int                   atype[],
              int                   asize[] )

    {       /*  begin body of  inqatt3c() */
    int  i, result ;
    FINT maxa, numa, atyp[ MXATTS3 ] , asiz[ MXATTS3 ]  ;

    maxa  = (FINT) maxatt ;
    result = (int) INQATT3( fname , (int) STRLEN( fname ) ,
                            vname , (int) STRLEN( vname ) ,
                            & maxa ,
                            & numa ,
                            attname , attnamelen ,
                            atype ,
                            asize ) ) ;

    if ( result )
        {
        * numatts = numa ;
        for( i = 0 ; i < numa ; i++ )
            {
            atype[ i ] = (int) atyp[ i ] ;
            asize[ i ] = (int) asiz[ i ] ;
            }
        }

    return result ;

    }       /*  end body of  inqatt3c()  */


                	/** END  CASE OF Win32 **/
                	/** NEXT CASE:  CRAY-TARGETED inqatt3c(): **/

#elif  defined(_CRAY)   /** treatment:  CRAY arrys of strings?? **/

#if( 0 )    /** if 0 **/

#include <fortran.h>


    extern FINT INQATT3( const _fcd   fname ,
                         const _fcd   vname ,
                         const FINT * mxatts,
                         FINT       * natts,
                         const _fcd   aname[],
                         FINT         atypes[],
                         FINT         asizes[] ) ;

int inqatt3c( const char          * fname ,
              const char          * vname ,
              int                   maxatt ,
              int                   attnamelen ,
              int                 * numatts ,
              char                * attname,
              int                   atype[],
              int                   asize[] )

    {       /*  begin body of  inqatt3c() */

    _fcd  file ;
    _fcd  vble ;
    _fcd  atts ;
    _fcd  att  ;
    long  stat ;
    char *cptr ;
    int   i, result ;
    FINT  maxa, numa, atyp[ MXATTS3 ] , asiz[ MXATTS3 ]  ;

    atts = (fcd_[]) malloc( maxatt * BUFLEN ) ;
    if ( ! atts ) return ( _btol( 0 ) ) ;

    maxa  = (FINT) maxatt ;
    file = _cptofcd( (char *)fname, (int) STRLEN( fname ) ) ;
    vble = _cptofcd( (char *)vname, (int) STRLEN( vname ) ) ;

    stat = INQATT3( file,
                    vble,
                  & maxa, 
                  & numa, 
                    atts, 
                    atype, 
                    asize ) ;

    if ( result = _btol( stat ) )
        {
        * numatts = numa ;
        for ( i = 0, att = atts , cptr = attname ; 
              i < numa ; 
              i++, att++, cptr += attnamelen )
            {
            fstr2cstr( cptr, attnamelen ) ;
            atype[ i ] = (int) atyp[ i ] ;
            asize[ i ] = (int) asiz[ i ] ;
            } ;
        } ;

    return result ;

    }       /*  end body of  inqatt3c()  */

#endif              /** if 0 **/

#else

#error   "Error compiling inqatt3c():  unsupported architecture"

#endif              /** IF FELDMAN-DESCENDED "FC" TARGETED, 
                        OR IF Win32, OR IF CRAY **/



