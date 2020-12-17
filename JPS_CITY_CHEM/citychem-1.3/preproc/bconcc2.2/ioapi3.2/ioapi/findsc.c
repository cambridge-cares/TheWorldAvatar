/**************************************************************************
VERSION "$Id: findsc.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE
    Return subscript at which integer entry found in SORTED 
    key-tables -- versions for single-key through quadruple-key
    tables, 
    Returns negative number in case of failure.

PRECONDITIONS

    Sorted tables list<n>

CALLS
    none

REVISION HISTORY
    prototype  3/1995 by CJC

    Version    8/1999 by CJC:  FLDMN, WIN32 stuff

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications
               
    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
    
**************************************************************************/

#include  "parms3.h"
#include  "iodecl3.h"

#if FLDMN

#define  FIND1  find1_
#define  FIND2  find2_
#define  FIND3  find3_
#define  FIND4  find4_

#define  FINDR1  findr1_
#define  FINDR2  findr2_
#define  FINDR3  findr3_
#define  FINDR4  findr4_

#elif defined(__hpux) || defined(_AIX)

#define  FIND1  find1
#define  FIND2  find2
#define  FIND3  find3
#define  FIND4  find4

#define  FINDR1  findr1
#define  FINDR2  findr2
#define  FINDR3  findr3
#define  FINDR4  findr4

#elif  defined(_CRAY) || defined(_WIN32) || defined(ABSFT)

#else
 
#error   "Error compiling findsc():  unsupported architecture"
 
#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/
                                        /** I/O API functions **/


    extern FINT FIND1( FINT  *k1, 
                       FINT  *n , 
                       const FINT *list1 ) ;

    extern FINT FIND2( FINT  *k1, 
                       FINT  *k2, 
                       FINT  *n , 
                       const FINT *list1 ,
                       const FINT *list2 ) ;

    extern FINT FIND3( FINT  *k1, 
                       FINT  *k2, 
                       FINT  *k3, 
                       FINT  *n , 
                       const FINT *list1 ,
                       const FINT *list2 ,
                       const FINT *list3 ) ;

    extern FINT FIND4( FINT  *k1, 
                       FINT  *k2, 
                       FINT  *k3, 
                       FINT  *k4, 
                       FINT  *n , 
                       const FINT *list1 ,
                       const FINT *list2 ,
                       const FINT *list3 ,
                       const FINT *list4 ) ;

    extern FINT FINDR1( FREAL *k1, 
                        FINT   *n , 
                        const FREAL *list1 ) ;

    extern FINT FINDR2( FREAL *k1, 
                        FREAL *k2, 
                        FINT   *n , 
                        const FREAL *list1 ,
                        const FREAL *list2 ) ;

    extern FINT FINDR3( FREAL *k1, 
                        FREAL *k2, 
                        FREAL *k3, 
                        FINT   *n , 
                        const FREAL *list1 ,
                        const FREAL *list2 ,
                        const FREAL *list3 ) ;

    extern FINT FINDR4( FREAL *k1, 
                        FREAL *k2, 
                        FREAL *k3, 
                        FREAL *k4, 
                        FINT   *n , 
                        const FREAL *list1 ,
                        const FREAL *list2 ,
                        const FREAL *list3 ,
                        const FREAL *list4 ) ;


FINT find1c( FINT        k1,      /** first  key component **/
            FINT        n,       /** table size **/
            const FINT *list1 )  /** first  key table **/
    {
    return -1 + FIND1( &k1, &n, list1 ) ;
    }                           /*  end body of find1c()  */

FINT find2c( FINT        k1,      /** first  key component **/
             FINT        k2,      /** second key component **/
             FINT        n,       /** table size **/
             const FINT *list1 ,  /** first  key table **/
             const FINT *list2 )  /** second key table **/
    {
    return -1 + FIND2( &k1, &k2, &n, list1, list2 ) ;
    }                           /*  end body of find2c()  */

FINT find3c( FINT        k1,      /** first  key component **/
             FINT        k2,      /** second key component **/
             FINT        k3,      /** third  key component **/
             FINT        n,       /** table size **/
             const FINT *list1 ,  /** first  key table **/
             const FINT *list2 ,  /** second key table **/
             const FINT *list3 )  /** third  key table **/
    {
    return -1 + FIND3( &k1, &k2, &k3, &n, list1, list2, list3 ) ;
    }                           /*  end body of find3c()  */

FINT find4c( FINT        k1,      /** first  key component **/
             FINT        k2,      /** second key component **/
             FINT        k3,      /** third  key component **/
             FINT        k4,      /** fourth key component **/
             FINT        n,       /** table size **/
             const FINT *list1 ,  /** first  key table **/
             const FINT *list2 ,  /** second key table **/
             const FINT *list3 ,  /** third  key table **/
             const FINT *list4 )  /** fourth key table **/
    {
    return -1 + FIND4( &k1, &k2, &k3, &k4, &n, list1, list2, list3, list4 ) ;
    }                           /*  end body of find4c()  */


FINT findr1c( FREAL        k1,      /** first  key component **/
              FINT          n,       /** table size **/
              const FREAL *list1 )  /** first  key table **/
    {
    return -1 + FINDR1( &k1, &n, list1 ) ;
    }                           /*  end body of findr1c()  */

FINT findr2c( FREAL        k1,      /** first  key component **/
              FREAL        k2,      /** second key component **/
              FINT          n,       /** table size **/
              const FREAL *list1 ,  /** first  key table **/
              const FREAL *list2 )  /** second key table **/
    {
    return -1 + FINDR2( &k1, &k2, &n, list1, list2 ) ;
    }                           /*  end body of findr2c()  */

FINT findr3c( FREAL        k1,      /** first  key component **/
              FREAL        k2,      /** second key component **/
              FREAL        k3,      /** third  key component **/
              FINT          n,       /** table size **/
              const FREAL *list1 ,  /** first  key table **/
              const FREAL *list2 ,  /** second key table **/
              const FREAL *list3 )  /** third  key table **/
    {
    return -1 + FINDR3( &k1, &k2, &k3, &n, list1, list2, list3 ) ;
    }                           /*  end body of findr3c()  */

FINT findr4c( FREAL        k1,      /** first  key component **/
              FREAL        k2,      /** second key component **/
              FREAL        k3,      /** third  key component **/
              FREAL        k4,      /** fourth key component **/
              FINT         n,       /** table size **/
              const FREAL *list1 ,  /** first  key table **/
              const FREAL *list2 ,  /** second key table **/
              const FREAL *list3 ,  /** third  key table **/
              const FREAL *list4 )  /** fourth key table **/
    {
    return -1 + FINDR4( &k1, &k2, &k3, &k4, &n, list1, list2, list3, list4 ) ;
    }                           /*  end body of findr4c()  */


