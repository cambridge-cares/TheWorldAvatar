
/**************************************************************************
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "locatsc.c" version "$Id: locatsc.c 1 2017-06-10 18:05:20Z coats $"

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

CALLS:
    I/O API Fortran-binding routines 
    LOCAT1,  LOCAT2,  LOCAT3,  LOCAT4,
    LOCATR1, LOCATR2, LOCATR3, LOCATR4

REVISION HISTORY
    prototype  3/1995 by CJC

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  "iodecl3.h"

#if FLDMN

#define  LOCAT1  locat1_
#define  LOCAT2  locat2_
#define  LOCAT3  locat3_
#define  LOCAT4  locat4_

#define  LOCATR1  locatr1_
#define  LOCATR2  locatr2_
#define  LOCATR3  locatr3_
#define  LOCATR4  locatr4_

#elif defined(__hpux) || defined(_AIX)

#define  LOCAT1  locat1
#define  LOCAT2  locat2
#define  LOCAT3  locat3
#define  LOCAT4  locat4

#define  LOCATR1  locatr1
#define  LOCATR2  locatr2
#define  LOCATR3  locatr3
#define  LOCATR4  locatr4

#elif  defined(_CRAY) || defined(_WIN32) || defined(ABSFT)

#else
 
#error   "Error compiling locatsc():  unsupported architecture"
 
#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/
                                        /** I/O API functions **/


    extern FINT LOCAT1( FINT  *k1, 
                        FINT  *n , 
                        const FINT *list1 ) ;

    extern FINT LOCAT2( FINT  *k1, 
                        FINT  *k2, 
                        FINT  *n , 
                        const FINT *list1 ,
                        const FINT *list2 ) ;

    extern FINT LOCAT3( FINT  *k1, 
                        FINT  *k2, 
                        FINT  *k3, 
                        FINT  *n , 
                        const FINT *list1 ,
                        const FINT *list2 ,
                        const FINT *list3 ) ;

    extern FINT LOCAT4( FINT  *k1, 
                        FINT  *k2, 
                        FINT  *k3, 
                        FINT  *k4, 
                        FINT  *n , 
                        const FINT *list1 ,
                        const FINT *list2 ,
                        const FINT *list3 ,
                        const FINT *list4 ) ;

    extern FINT LOCATR1( FREAL *k1, 
                         FINT   *n , 
                         const FREAL *list1 ) ;

    extern FINT LOCATR2( FREAL *k1, 
                         FREAL *k2, 
                         FINT   *n , 
                         const FREAL *list1 ,
                         const FREAL *list2 ) ;

    extern FINT LOCATR3( FREAL *k1, 
                         FREAL *k2, 
                         FREAL *k3, 
                         FINT   *n , 
                         const FREAL *list1 ,
                         const FREAL *list2 ,
                         const FREAL *list3 ) ;

    extern FINT LOCATR4( FREAL *k1, 
                         FREAL *k2, 
                         FREAL *k3, 
                         FREAL *k4, 
                         FINT   *n , 
                         const FREAL *list1 ,
                         const FREAL *list2 ,
                         const FREAL *list3 ,
                         const FREAL *list4 ) ;


int locat1c( int        k1,      /** first  key component **/
             int        n,       /** table size **/
             const int *list1 )  /** first  key table **/
    {
    int ll ;
    ll = (int) LOCAT1( &k1, &n, list1 ) ;
    return ( ll > 0 ? -1 + ll : -1 ) ;
    }                           /*  end body of locat1c()  */

int locat2c( int        k1,      /** first  key component **/
             int        k2,      /** second key component **/
             int        n,       /** table size **/
             const int *list1 ,  /** first  key table **/
             const int *list2 )  /** second key table **/
    {
    int ll ;
    ll = (int) LOCAT2( &k1, &k2, &n, list1, list2 ) ;
    return ( ll > 0 ? -1 + ll : -1 ) ;
    }                           /*  end body of locat2c()  */

int locat3c( int        k1,      /** first  key component **/
             int        k2,      /** second key component **/
             int        k3,      /** third  key component **/
             int        n,       /** table size **/
             const int *list1 ,  /** first  key table **/
             const int *list2 ,  /** second key table **/
             const int *list3 )  /** third  key table **/
    {
    int ll ;
    ll = (int) LOCAT3( &k1, &k2, &k3, &n, list1, list2, list3 ) ;
    return ( ll > 0 ? -1 + ll : -1 ) ;
    }                           /*  end body of locat3c()  */

int locat4c( int        k1,      /** first  key component **/
             int        k2,      /** second key component **/
             int        k3,      /** third  key component **/
             int        k4,      /** fourth key component **/
             int        n,       /** table size **/
             const int *list1 ,  /** first  key table **/
             const int *list2 ,  /** second key table **/
             const int *list3 ,  /** third  key table **/
             const int *list4 )  /** fourth key table **/
    {
    int ll ;
    ll = (int) LOCAT4( &k1, &k2, &k3, &k4, &n, list1, list2, list3, list4 ) ;
    return ( ll > 0 ? -1 + ll : -1 ) ;
    }                           /*  end body of locat4c()  */


int locatr1c( FREAL        k1,      /** first  key component **/
              int          n,       /** table size **/
              const FREAL *list1 )  /** first  key table **/
    {
    int ll ;
    ll = (int) LOCATR1( &k1, &n, list1 ) ;
    return ( ll > 0 ? -1 + ll : -1 ) ;
    }                           /*  end body of locatr1c()  */

int locatr2c( FREAL        k1,      /** first  key component **/
              FREAL        k2,      /** second key component **/
              int          n,       /** table size **/
              const FREAL *list1 ,  /** first  key table **/
              const FREAL *list2 )  /** second key table **/
    {
    int ll ;
    ll = (int) LOCATR2( &k1, &k2, &n, list1, list2 ) ;
    return ( ll > 0 ? -1 + ll : -1 ) ;
    }                           /*  end body of locatr2c()  */

int locatr3c( FREAL        k1,      /** first  key component **/
              FREAL        k2,      /** second key component **/
              FREAL        k3,      /** third  key component **/
              int          n,       /** table size **/
              const FREAL *list1 ,  /** first  key table **/
              const FREAL *list2 ,  /** second key table **/
              const FREAL *list3 )  /** third  key table **/
    {
    int ll ;
    ll = (int) LOCATR3( &k1, &k2, &k3, &n, list1, list2, list3 ) ;
    return ( ll > 0 ? -1 + ll : -1 ) ;
    }                           /*  end body of locatr3c()  */

int locatr4c( FREAL        k1,      /** first  key component **/
              FREAL        k2,      /** second key component **/
              FREAL        k3,      /** third  key component **/
              FREAL        k4,      /** fourth key component **/
              int          n,       /** table size **/
              const FREAL *list1 ,  /** first  key table **/
              const FREAL *list2 ,  /** second key table **/
              const FREAL *list3 ,  /** third  key table **/
              const FREAL *list4 )  /** fourth key table **/
    {
    int ll ;
    ll = (int) LOCATR4( &k1, &k2, &k3, &k4, &n, list1, list2, list3, list4 ) ;
    return ( ll > 0 ? -1 + ll : -1 ) ;
    }                           /*  end body of locatr4c()  */


