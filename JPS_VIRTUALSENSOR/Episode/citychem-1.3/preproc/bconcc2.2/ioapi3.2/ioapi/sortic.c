
/***********************************************************************
   void qsortic () for    C   starts at line   67
   void qsortic8() for    C   starts at line  166
   void SORTIC  () for Fortran starts at line 267
   void SORTIC8 () for Fortran starts at line 296

VERSION "$Id: sortic.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    (C) 2003-2010 Baron Advanced Meteorological Systems,
    (C) 2014 UNC Institute for the Environment.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    qsortic() and SORTIC() sort index-tables ind[] on the basis of
    Fortran character-string arrays holding keys.  SORTIC() is
    designed to be called from Fortran, so its subscripting from ind[]
    is offset by 1 for indexing into key tables  tblc[]

ALGORITHM:
    quick sort (q.v. Sedgwick, _Algorithms_, 2nd. ed., chapter 9)

PRECONDITIONS:
    ind[ N ] initialized with 1, 2, ..., N      for SORTIC(),
                              0, k, ..., (N-1)k for qsortic()

REVISION HISTORY:
    Prototypes 8/1996 by CJC

    Version    8/1999 by CJC:  FLDMN, WIN32 portability enhancements

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Version    3/2014 by CJC:  Add 64-bit-subscripting SORTIC8() and
    32-bit SORTIC4() for generic interface in MODULE M3UTILIO;
    SORTIC() uses SORTIC4() for EXTERNAL references.

    Version    6/2014 by CJC:  add specified-length-comparison routines
    SORTINC4(), SORTINC8()
************************************************************************/

#include  <string.h>
#include  <stdint.h>
#include "parms3.h"

                         /**  DEAL WITH  FELDMANISMS  OF MANY UN*X F77'S   **/

#if FLDMN

#define  SORTIC    sortic_
#define  SORTIC4   sortic4_
#define  SORTIC8   sortic8_
#define  SORTINC4  sortinc4_
#define  SORTINC8  sortinc8_

#elif defined(__hpux) || defined(_AIX)

#define  SORTIC    sortic
#define  SORTIC4   sortic4
#define  SORTIC8   sortic8
#define  SORTINC4  sortinc4
#define  SORTINC8  sortinc8

#endif                                   /** #IF SGI OR SUN OR OSF OR MIPS **/


                    /** MACROS FOR COMPARING INDICES INTO KEY-TUPLE TABLES9 **/
        /** CMPk( X,Y ) iff index X > index Y in terms of k-tuple tables   **/
        /** i.e.,  1  iff *out*of order,  -1 iff *in*order,  0 if *equal*  **/

#define CMPC( X,Y )   strncmp( tblc + (X) , tblc + (Y) , (size_t) k )


/********************* BODIES OF THE PRIVATE SORT-ROUTINES *******************/

void qsortic( int          n,        /** number of elements              **/
              FINT         ind[],    /** index-array                     **/
              const char   tblc[],   /** first  key-component in tuple   **/
              const FSTR_L k )       /** key-length as a Fortran string  **/
    {
    int  a, b, c ;
    int  l, r , p ;
    int  t, u, v, w ;

    if ( n > 2 )
        {
                /** DO SORT-3 TO GET MEDIAN-OF-3  -- Q.V.KNUTH VOL3 P. 182 **/
        p = n / 2 ;
        a = ind[ 0 ] ;
        b = ind[ p ] ;
        c = ind[ n-1 ] ;
        u = CMPC( a,b ) ;
        v = CMPC( b,c ) ;

        if ( u > 0 )                                       /** A,B REVERSED **/
            {
            if ( v > 0 )                                   /** ABC ~~~> CBA **/
                {
                ind[ 0   ] = c ;
                ind[ n-1 ] = a ;
                }
            else{
                w = CMPC( a,c ) ;
                if ( w > 0 )                               /** ABC ~~~> BCA **/
                    {
                    ind[ 0   ] = b ;
                    ind[ p   ] = c ;
                    ind[ n-1 ] = a ;
                    }
                else{                                      /** ABC ~~~> BAC **/
                    ind[ 0   ] = b ;
                    ind[ p   ] = a ;
                    }
                }
            }
        else if ( v > 0 )                           /** A,B OK; BC REVERSED **/
            {
            w = CMPC( a,c ) ;
            if ( w > 0 )                                   /** ABC ~~~> CAB **/
                {
                ind[ 0   ] = c ;
                ind[ p   ] = a ;
                ind[ n-1 ] = b ;
                }
            else{                                          /** ABC ~~~> ACB **/
                ind[ p   ] = c ;
                ind[ n-1 ] = b ;
                }
            }

                                        /** IF N > 3, PARTITION AND RECURSE **/
        if ( n > 3 )
            {
            b        = ind[ p ] ;
            ind[ p ] = ind[ 0 ] ;
            ind[ 0 ] = b ;

            for( l = 1, r = n-1 ; ; l++, r-- )
                {
                for( ; CMPC( ind[l], b ) < 0 ; l++ ) ;       /** EMPTY BODY **/

                for( ; CMPC( ind[r], b ) > 0 ; r-- ) ;       /** EMPTY BODY **/

                if ( l < r )
                    {
                    t        = ind[ l ] ;
                    ind[ l ] = ind[ r ] ;
                    ind[ r ] = t ;
                    }
                else break ;
                }

            ind[ 0 ] = ind[ r ] ;
            ind[ r ] = b ;

            qsortic( r,   ind,   tblc, k ) ;
            qsortic( n-l, ind+l, tblc, k ) ;
            }                                     /** END IF-CLAUSE:  N > 3 **/
        }                                         /** END IF-CLAUSE:  N > 2 **/

    else if ( n == 2 )
        {
        a = ind[ 0 ] ;
        b = ind[ 1 ] ;
        if ( CMPC( a,b ) > 0 )
            {
            ind[ 0 ] = b ;
            ind[ 1 ] = a ;
            }
        }                                     /** END ELSE-IF CLAUSE:  N==2 **/

    }   /** .............................................END VOID qsortic() **/


void qsortic8( int64_t         n,    /** number of elements              **/
               int64_t         ind[], /** index-array                     **/
               const char   tblc[],   /** first  key-component in tuple   **/
               const FSTR_L k )       /** key-length as a Fortran string  **/
    {
    int64_t  a, b, c ;
    int64_t  l, r , p ;
    int64_t  t, u, v, w ;

    if ( n > 2 )
        {
                /** DO SORT-3 TO GET MEDIAN-OF-3  -- Q.V.KNUTH VOL3 P. 182 **/
        p = n / 2 ;
        a = ind[ 0 ] ;
        b = ind[ p ] ;
        c = ind[ n-1 ] ;
        u = CMPC( a,b ) ;
        v = CMPC( b,c ) ;

        if ( u > 0 )                                       /** A,B REVERSED **/
            {
            if ( v > 0 )                                   /** ABC ~~~> CBA **/
                {
                ind[ 0   ] = c ;
                ind[ n-1 ] = a ;
                }
            else{
                w = CMPC( a,c ) ;
                if ( w > 0 )                               /** ABC ~~~> BCA **/
                    {
                    ind[ 0   ] = b ;
                    ind[ p   ] = c ;
                    ind[ n-1 ] = a ;
                    }
                else{                                      /** ABC ~~~> BAC **/
                    ind[ 0   ] = b ;
                    ind[ p   ] = a ;
                    }
                }
            }
        else if ( v > 0 )                           /** A,B OK; BC REVERSED **/
            {
            w = CMPC( a,c ) ;
            if ( w > 0 )                                   /** ABC ~~~> CAB **/
                {
                ind[ 0   ] = c ;
                ind[ p   ] = a ;
                ind[ n-1 ] = b ;
                }
            else{                                          /** ABC ~~~> ACB **/
                ind[ p   ] = c ;
                ind[ n-1 ] = b ;
                }
            }

                                        /** IF N > 3, PARTITION AND RECURSE **/
        if ( n > 3 )
            {
            b        = ind[ p ] ;
            ind[ p ] = ind[ 0 ] ;
            ind[ 0 ] = b ;

            for( l = 1, r = n-1 ; ; l++, r-- )
                {
                for( ; CMPC( ind[l], b ) < 0L ; l++ ) ;       /** EMPTY BODY **/
                for( ; CMPC( ind[r], b ) > 0L ; r-- ) ;       /** EMPTY BODY **/

                if ( l < r )
                    {
                    t        = ind[ l ] ;
                    ind[ l ] = ind[ r ] ;
                    ind[ r ] = t ;
                    }
                else break ;
                }

            ind[ 0 ] = ind[ r ] ;
            ind[ r ] = b ;

            qsortic8( r,   ind,   tblc, k ) ;
            qsortic8( n-l, ind+l, tblc, k ) ;
            }                                     /** END IF-CLAUSE:  N > 3 **/
        }                                         /** END IF-CLAUSE:  N > 2 **/

    else if ( n == 2 )
        {
        a = ind[ 0 ] ;
        b = ind[ 1 ] ;
        if ( CMPC( a,b ) > 0 )
            {
            ind[ 0 ] = b ;
            ind[ 1 ] = a ;
            }
        }                                     /** END ELSE-IF CLAUSE:  N==2 **/

    }   /** .............................................END VOID qsortic() **/


/**********************   BODY OF THE F77 SORT-ROUTINES  *******************/

void SORTIC4( const FINT  * nelts,      /** number of elements              **/
              FINT          ind[],      /** index-array                     **/
              const char    tblc[],     /** table: key-component in 1-tuple **/
              const FSTR_L  k )         /** string length in table          **/
    {
    int n, i ;

    n = (int) *nelts ;

                              /** CONVERT FROM FORTRAN SUBSCRIPTS TO C SUBS **/
    for ( i = 0 ; i < n ; i++ )           /** ALSO ADJUST FOR STRING LENGTH **/
        {
        ind[ i ] = k * ( ind[ i ] - 1 ) ;
        } ;

                                             /** CALL C QSORT ROUTINE **/
    qsortic( n, ind, tblc, k ) ;

                         /** CONVERT FROM C SUBSCRIPTS BACK TO FORTRAN SUBS **/
    for ( i = 0 ; i < n ; i++ )        /** ALSO DE-ADJUST FOR STRING LENGTH **/
        {
        ind[ i ] = 1 + ( ind[ i ] / k ) ;
        }

    return ;

    } /*********************************************  END FUNCTION SORTIC4() **/


void SORTIC8( const int64_t  * nelts,      /** number of elements              **/
              int64_t          ind[],      /** index-array                     **/
              const char       tblc[],     /** table: key-component in 1-tuple **/
              const FSTR_L     k )         /** string length in table          **/
    {
    int64_t     i, j, n ;

    j = (int64_t) k ;
    n = (int64_t) *nelts ;

                              /** CONVERT FROM FORTRAN SUBSCRIPTS TO C SUBS **/
    for ( i = 0 ; i < n ; i++ )           /** ALSO ADJUST FOR STRING LENGTH **/
        {
        if ( ind[ i ] > n )  exit( 2 ) ;
        ind[ i ] = j * ( ind[ i ] - 1 ) ;
        } ;

                                             /** CALL C QSORT ROUTINE **/
    qsortic8( n, ind, tblc, k ) ;

                         /** CONVERT FROM C SUBSCRIPTS BACK TO FORTRAN SUBS **/
    for ( i = 0 ; i < n ; i++ )        /** ALSO DE-ADJUST FOR STRING LENGTH **/
        {
        ind[ i ] = 1 + ( ind[ i ] / j ) ;
        }

    return ;

    } /*********************************************  END FUNCTION SORTIC8() **/


/**********************   BODY OF THE F77 SORT-ROUTINES  *******************/

void SORTINC4( const FINT  * nelts,      /** number of elements              **/
               const FINT  * nlens,      /** number of sorted characters     **/
               FINT          ind[],      /** index-array                     **/
               const char    tblc[],     /** table: key-component in 1-tuple **/
               const FSTR_L  k )         /** string length in table          **/
    {
    int i, m, n ;

    n = (int) *nelts ;
    m = (int) *nlens ;

                              /** CONVERT FROM FORTRAN SUBSCRIPTS TO C SUBS **/
    for ( i = 0 ; i < n ; i++ )           /** ALSO ADJUST FOR STRING LENGTH **/
        {
        ind[ i ] = k * ( ind[ i ] - 1 ) ;
        } ;

                                             /** CALL C QSORT ROUTINE **/
    qsortic( n, ind, tblc, m ) ;

                         /** CONVERT FROM C SUBSCRIPTS BACK TO FORTRAN SUBS **/
    for ( i = 0 ; i < n ; i++ )        /** ALSO DE-ADJUST FOR STRING LENGTH **/
        {
        ind[ i ] = 1 + ( ind[ i ] / k ) ;
        }

    return ;

    } /*********************************************  END FUNCTION SORTINC4() **/


void SORTINC8( const int64_t  * nelts,      /** number of elements              **/
               const int64_t  * nlens,      /** number of sorted characters     **/
               int64_t          ind[],      /** index-array                     **/
               const char       tblc[],     /** table: key-component in 1-tuple **/
               const FSTR_L     k )         /** string length in table          **/
    {
    int64_t     i, j, m, n ;

    j = (int64_t) k ;
    n = (int64_t) *nelts ;
    m = (int64_t) *nlens ;

                              /** CONVERT FROM FORTRAN SUBSCRIPTS TO C SUBS **/
    for ( i = 0 ; i < n ; i++ )           /** ALSO ADJUST FOR STRING LENGTH **/
        {
        if ( ind[ i ] > n )  exit( 2 ) ;
        ind[ i ] = j * ( ind[ i ] - 1 ) ;
        } ;

                                             /** CALL C QSORT ROUTINE **/
    qsortic8( n, ind, tblc, m ) ;

                         /** CONVERT FROM C SUBSCRIPTS BACK TO FORTRAN SUBS **/
    for ( i = 0 ; i < n ; i++ )        /** ALSO DE-ADJUST FOR STRING LENGTH **/
        {
        ind[ i ] = 1 + ( ind[ i ] / j ) ;
        }

    return ;

    } /*********************************************  END FUNCTION SORTINC8() **/


void SORTIC( const FINT  * nelts,      /** number of elements              **/
             FINT          ind[],      /** index-array                     **/
             const char    tblc[],     /** table: key-component in 1-tuple **/
             const FSTR_L  k )         /** string length in table          **/
    {
    SORTIC4( nelts, ind, tblc, k ) ;
    return ;

    } /*********************************************  END FUNCTION SORTIC() **/


