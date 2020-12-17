
/***********************************************************************
   void qsortr1() for    C   starts at line 103
   void qsortr2() for    C   starts at line 205
   void qsortr3() for    C   starts at line 309
   void qsortr4() for    C   starts at line 415
   void SORTR1() for Fortran starts at line 525
   void SORTR2() for Fortran starts at line 553
   void SORTR3() for Fortran starts at line 581
   void SORTR4() for Fortran starts at line 611

VERSION "$Id: sortir.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    qsortr<k>() and SORTR<k>() sort index-tables ind[] on the basis of 
    <k> parallel arrays holding <k>-tuple keys.  The SORTR<k>() are 
    designed to be called from Fortran, so their subscripting from ind[] 
    is offset by 1 for indexing into key tables  tbl<k>[]

ALGORITHM:
    quick sort (q.v. Sedgwick, _Algorithms_, 2nd. ed., chapter 9)

PRECONDITIONS:
    ind[ N ] initialized with 1, ..., N  
    (Fortran-style subscripts to the key tables tbl<k>[])

REVISION HISTORY:
    Prototypes 9/95 by CJC

    Version    8/99 by CJC:  FLDMN, WIN32 portability enhancements

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

****************************************************************************/

#include   "parms3.h"
 
                         /**  DEAL WITH  FELDMANISMS  OF MANY UN*X F77'S   **/
 
#if FLDMN
 
#define  SORTR1   sortr1_
#define  SORTR2   sortr2_
#define  SORTR3   sortr3_
#define  SORTR4   sortr4_

#elif defined(__hpux) || defined(_AIX)

#define  SORTR1   sortr1
#define  SORTR2   sortr2
#define  SORTR3   sortr3
#define  SORTR4   sortr4

#endif                                   /** #IF SGI OR SUN OR OSF OR MIPS **/


                    /** MACROS FOR COMPARING INDICES INTO KEY-TUPLE TABLES **/
        /** CMPk( X,Y ) iff index X > index Y in terms of k-tuple tables   **/
        /** LCMPk( I,K1, ...,Kk ) iff k-tuple[ ind[ I ] ] > (Y1, ..., Yk)  **/
        /** i.e.,  1  iff *out*of order,  -1 iff *in*order,  0 if *equal*  **/

#define CMP1( X,Y ) \
 ( tbl1[ X ] > tbl1[ Y ] ? 1 : ( tbl1[ X ] < tbl1[ Y ] ? -1 : 0 ) )

#define CMP2( X,Y ) \
 ( tbl1[ X ] > tbl1[ Y ] ? 1 : ( tbl1[ X ] < tbl1[ Y ] ? -1 : \
 ( tbl2[ X ] > tbl2[ Y ] ? 1 : ( tbl2[ X ] < tbl2[ Y ] ? -1 : 0 ) ) ) )

#define CMP3( X,Y ) \
 ( tbl1[ X ] > tbl1[ Y ] ? 1 : ( tbl1[ X ] < tbl1[ Y ] ? -1 : \
 ( tbl2[ X ] > tbl2[ Y ] ? 1 : ( tbl2[ X ] < tbl2[ Y ] ? -1 : \
 ( tbl3[ X ] > tbl3[ Y ] ? 1 : ( tbl3[ X ] < tbl3[ Y ] ? -1 : 0 ) ) ) ) ) )

#define CMP4( X,Y ) \
 ( tbl1[ X ] > tbl1[ Y ] ? 1 : ( tbl1[ X ] < tbl1[ Y ] ? -1 : \
 ( tbl2[ X ] > tbl2[ Y ] ? 1 : ( tbl2[ X ] < tbl2[ Y ] ? -1 : \
 ( tbl3[ X ] > tbl3[ Y ] ? 1 : ( tbl3[ X ] < tbl3[ Y ] ? -1 : \
 ( tbl4[ X ] > tbl4[ Y ] ? 1 : ( tbl4[ X ] < tbl4[ Y ] ? -1 : 0 ) ) ) ) ) ) ) )


#define LCMP1( I,K1 ) \
 ( tbl1[ ind[ I ] ] > K1 ? 1 : ( tbl1[ ind[ I ] ] < K1 ? -1 : 0 ) )

#define LCMP2( I,K1,K2 ) \
 ( tbl1[ ind[ I ] ] > K1 ? 1 : ( tbl1[ ind[ I ] ] < K1 ? -1 : \
 ( tbl2[ ind[ I ] ] > K2 ? 1 : ( tbl2[ ind[ I ] ] < K2 ? -1 : 0 ) ) ) )

#define LCMP3( I,K1,K2,K3 ) \
 ( tbl1[ ind[ I ] ] > K1 ? 1 : ( tbl1[ ind[ I ] ] < K1 ? -1 : \
 ( tbl2[ ind[ I ] ] > K2 ? 1 : ( tbl2[ ind[ I ] ] < K2 ? -1 : \
 ( tbl3[ ind[ I ] ] > K3 ? 1 : ( tbl3[ ind[ I ] ] < K3 ? -1 : 0 ) ) ) ) ) )

#define LCMP4( I,K1,K2,K3,K4 ) \
 ( tbl1[ ind[ I ] ] > K1 ? 1 : ( tbl1[ ind[ I ] ] < K1 ? -1 : \
 ( tbl2[ ind[ I ] ] > K2 ? 1 : ( tbl2[ ind[ I ] ] < K2 ? -1 : \
 ( tbl3[ ind[ I ] ] > K3 ? 1 : ( tbl3[ ind[ I ] ] < K3 ? -1 : \
 ( tbl4[ ind[ I ] ] > K4 ? 1 : ( tbl4[ ind[ I ] ] < K4 ? -1 : 0 ) ) ) ) ) ) ) )


/********************* BODIES OF THE PRIVATE SORT-ROUTINES *******************/

void qsortr1( FINT        n,          /** number of elements             **/
              FINT        ind[],      /** index-array                    **/
              const FREAL tbl1[] )    /** first  key-component in tuple  **/
    {
    FREAL k1 ;
    FINT  a, b, c ;
    FINT  l, r , p ;
    FINT  t, u, v, w ;
    
    if ( n > 2 )
        {
                /** DO SORT-3 TO GET MEDIAN-OF-3  -- Q.V.KNUTH VOL3 P. 182 **/
        p = n / 2 ;
        a = ind[ 0 ] ;
        b = ind[ p ] ;
        c = ind[ n-1 ] ;
        u = CMP1( a,b ) ;
        v = CMP1( b,c ) ;
        w = CMP1( a,c ) ;

        if ( u > 0 )                                       /** A,B REVERSED **/
            {
            if ( v > 0 )                                   /** ABC ~~~> CBA **/
                {
                ind[ 0   ] = c ;
                ind[ n-1 ] = a ;
                }
            else{
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
            b  = ind[  p ] ;
            k1 = tbl1[ b ] ;
            
            t        = ind[ p ] ;
            ind[ p ] = ind[ 0 ] ;
            ind[ 0 ] = t ;

            for( l = 1, r = n-1 ; ; l++, r-- )
                {
                for( ; LCMP1( l,k1 ) < 0 ; l++ ) ;           /** EMPTY BODY **/

                for( ; LCMP1( r,k1 ) > 0 ; r-- ) ;           /** EMPTY BODY **/

                if ( l < r )
                    {
                    t        = ind[ l ] ;
                    ind[ l ] = ind[ r ] ;
                    ind[ r ] = t ;
                    }
                else break ;
                }

            t        = ind[ r ] ;
            ind[ r ] = ind[ 0 ] ;
            ind[ 0 ] = t ;

            qsortr1( r,   ind,   tbl1 ) ;
            qsortr1( n-l, ind+l, tbl1 ) ;
            }                                     /** END IF-CLAUSE:  N > 3 **/
        }                                         /** END IF-CLAUSE:  N > 2 **/

    else if ( n == 2 )
        {
        a = ind[ 0 ] ;
        b = ind[ 1 ] ;
        if ( CMP1( a,b ) > 0 )
            {
            ind[ 0 ] = b ;
            ind[ 1 ] = a ;
            }
        }                                     /** END ELSE-IF CLAUSE:  N==2 **/

    }   /** .............................................END VOID qsortr1() **/


void qsortr2( FINT        n,           /** number of elements             **/
              FINT        ind[],       /** index-array                    **/
              const FREAL tbl1[],      /** first  key-component in tuple  **/
              const FREAL tbl2[] )     /** second key-component in tuple  **/
    {
    FREAL k1, k2 ;
    FINT  a, b, c ;
    FINT  l, r , p ;
    FINT  t, u, v, w ;
    
    if ( n > 2 )
        {
                /** DO SORT-3 TO GET MEDIAN-OF-3  -- Q.V.KNUTH VOL3 P. 182 **/
        p = n / 2 ;
        a = ind[ 0 ] ;
        b = ind[ p ] ;
        c = ind[ n-1 ] ;
        u = CMP2( a,b ) ;
        v = CMP2( b,c ) ;
        w = CMP2( a,c ) ;

        if ( u > 0 )                                       /** A,B REVERSED **/
            {
            if ( v > 0 )                                   /** ABC ~~~> CBA **/
                {
                ind[ 0   ] = c ;
                ind[ n-1 ] = a ;
                }
            else{
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
            b  = ind[  p ] ;
            k1 = tbl1[ b ] ;
            k2 = tbl2[ b ] ;
            
            t        = ind[ p ] ;
            ind[ p ] = ind[ 0 ] ;
            ind[ 0 ] = t ;

            for( l = 1, r = n-1 ; ; l++, r-- )
                {
                for( ; LCMP2( l,k1,k2 ) < 0 ; l++ ) ;        /** EMPTY BODY **/

                for( ; LCMP2( r,k1,k2 ) > 0 ; r-- ) ;        /** EMPTY BODY **/

                if ( l < r )
                    {
                    t        = ind[ l ] ;
                    ind[ l ] = ind[ r ] ;
                    ind[ r ] = t ;
                    }
                else break ;
                }

            t        = ind[ r ] ;
            ind[ r ] = ind[ 0 ] ;
            ind[ 0 ] = t ;

            qsortr2( r,   ind,   tbl1, tbl2 ) ;
            qsortr2( n-l, ind+l, tbl1, tbl2 ) ;
            }                                     /** END IF-CLAUSE:  N > 3 **/
        }                                         /** END IF-CLAUSE:  N > 2 **/

    else if ( n == 2 )
        {
        a = ind[ 0 ] ;
        b = ind[ 1 ] ;
        if ( CMP2( a,b ) > 0 )
            {
            ind[ 0 ] = b ;
            ind[ 1 ] = a ;
            }
        }                                     /** END ELSE-IF CLAUSE:  N==2 **/

    }   /** .............................................END VOID qsortr2() **/


void qsortr3( FINT        n,           /** number of elements             **/
              FINT        ind[],       /** index-array                    **/
              const FREAL tbl1[],      /** first  key-component in tuple  **/
              const FREAL tbl2[],      /** second key-component in tuple  **/
              const FREAL tbl3[] )     /** third  key-component in tuple  **/
    {
    FREAL k1, k2, k3 ;
    FINT  a, b, c ;
    FINT  l, r , p ;
    FINT  t, u, v, w ;
    
    if ( n > 2 )
        {
                /** DO SORT-3 TO GET MEDIAN-OF-3  -- Q.V.KNUTH VOL3 P. 182 **/
        p = n / 2 ;
        a = ind[ 0 ] ;
        b = ind[ p ] ;
        c = ind[ n-1 ] ;
        u = CMP3( a,b ) ;
        v = CMP3( b,c ) ;
        w = CMP3( a,c ) ;

        if ( u > 0 )                                       /** A,B REVERSED **/
            {
            if ( v > 0 )                                   /** ABC ~~~> CBA **/
                {
                ind[ 0   ] = c ;
                ind[ n-1 ] = a ;
                }
            else{
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
            b  = ind[  p ] ;
            k1 = tbl1[ b ] ;
            k2 = tbl2[ b ] ;
            k3 = tbl3[ b ] ;
            
            t        = ind[ p ] ;
            ind[ p ] = ind[ 0 ] ;
            ind[ 0 ] = t ;

            for( l = 1, r = n-1 ; ; l++, r-- )
                {
                for( ; LCMP3( l,k1,k2,k3 ) < 0 ; l++ ) ;     /** EMPTY BODY **/

                for( ; LCMP3( r,k1,k2,k3 ) > 0 ; r-- ) ;     /** EMPTY BODY **/

                if ( l < r )
                    {
                    t        = ind[ l ] ;
                    ind[ l ] = ind[ r ] ;
                    ind[ r ] = t ;
                    }
                else break ;
                }

            t        = ind[ r ] ;
            ind[ r ] = ind[ 0 ] ;
            ind[ 0 ] = t ;

            qsortr3( r,   ind,   tbl1, tbl2, tbl3 ) ;
            qsortr3( n-l, ind+l, tbl1, tbl2, tbl3 ) ;
            }                                     /** END IF-CLAUSE:  N > 3 **/
        }                                         /** END IF-CLAUSE:  N > 2 **/

    else if ( n == 2 )
        {
        a = ind[ 0 ] ;
        b = ind[ 1 ] ;
        if ( CMP3( a,b ) > 0 )
            {
            ind[ 0 ] = b ;
            ind[ 1 ] = a ;
            }
        }                                     /** END ELSE-IF CLAUSE:  N==2 **/

    }  /** ..............................................END VOID qsortr3() **/


void qsortr4( FINT        n,           /** number of elements             **/
              FINT        ind[],       /** index-array                    **/
              const FREAL tbl1[],      /** first  key-component in tuple  **/
              const FREAL tbl2[],      /** second key-component in tuple  **/
              const FREAL tbl3[],      /** third  key-component in tuple  **/
              const FREAL tbl4[] )     /** fourth key-component in tuple  **/
    {
    FREAL k1, k2, k3, k4 ;
    FINT  a, b, c ;
    FINT  l, r , p ;
    FINT  t, u, v, w ;
    
    if ( n > 2 )
        {
                /** DO SORT-3 TO GET MEDIAN-OF-3  -- Q.V.KNUTH VOL3 P. 182 **/
        p = n / 2 ;
        a = ind[ 0 ] ;
        b = ind[ p ] ;
        c = ind[ n-1 ] ;
        u = CMP4( a,b ) ;
        v = CMP4( b,c ) ;
        w = CMP4( a,c ) ;

        if ( u > 0 )                                       /** A,B REVERSED **/
            {
            if ( v > 0 )                                   /** ABC ~~~> CBA **/
                {
                ind[ 0   ] = c ;
                ind[ n-1 ] = a ;
                }
            else{
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
            b  = ind[  p ] ;
            k1 = tbl1[ b ] ;
            k2 = tbl2[ b ] ;
            k3 = tbl3[ b ] ;
            k4 = tbl4[ b ] ;
            
            t        = ind[ p ] ;
            ind[ p ] = ind[ 0 ] ;
            ind[ 0 ] = t ;

            for( l = 1, r = n-1 ; ; l++, r-- )
                {
                for( ; LCMP4( l,k1,k2,k3,k4 ) < 0 ; l++ ) ;  /** EMPTY BODY **/

                for( ; LCMP4( r,k1,k2,k3,k4 ) > 0 ; r-- ) ;  /** EMPTY BODY **/

                if ( l < r )
                    {
                    t        = ind[ l ] ;
                    ind[ l ] = ind[ r ] ;
                    ind[ r ] = t ;
                    }
                else break ;
                }

            t        = ind[ r ] ;
            ind[ r ] = ind[ 0 ] ;
            ind[ 0 ] = t ;

            qsortr4( r,   ind,   tbl1, tbl2, tbl3, tbl4 ) ;
            qsortr4( n-l, ind+l, tbl1, tbl2, tbl3, tbl4 ) ;
            }                                     /** END IF-CLAUSE:  N > 3 **/
        }                                         /** END IF-CLAUSE:  N > 2 **/

    else if ( n == 2 )
        {
        a = ind[ 0 ] ;
        b = ind[ 1 ] ;
        if ( CMP4( a,b ) > 0 )
            {
            ind[ 0 ] = b ;
            ind[ 1 ] = a ;
            }
        }                                     /** END ELSE-IF CLAUSE:  N==2 **/

    }  /**...............................................END VOID qsortr4() **/


/**********************   BODIES OF THE F77 SORT-ROUTINES   *******************/

void SORTR1( const FINT* nelts,          /** number of elements              **/
             FINT        ind[],          /** index-array                     **/
             const FREAL tbl1[] )        /** table: key-component in 1-tuple **/
    {
    FINT  n, i ;

    n = *nelts ;

                              /** CONVERT FROM FORTRAN SUBSCRIPTS TO C SUBS **/
    for ( i = 0 ; i < n ; i++ )
        {
        ind[ i ]-- ;
        } ;

                                             /** CALL C QSORT ROUTINE **/
    qsortr1( n, ind, tbl1 ) ;
        
                         /** CONVERT FROM C SUBSCRIPTS BACK TO FORTRAN SUBS **/
    for ( i = 0 ; i < n ; i++ )
        {
        ind[ i ]++ ;
        }

    return ;

    } /***********************************************  END FUNCTION SORTR) **/


void SORTR2( const FINT* nelts,    /** number of elements                     **/
             FINT        ind[],    /** index-array                            **/
             const FREAL tbl1[],   /** table:  first  key-component in tuple  **/
             const FREAL tbl2[] )  /** table:  second key-component in tuple  **/
    {    
    FINT  n, i ;

    n = *nelts ;

                              /** CONVERT FROM FORTRAN SUBSCRIPTS TO C SUBS **/
    for ( i = 0 ; i < n ; i++ )
        {
        ind[ i ]-- ;
        } ;

                                           /** CALL C-BINDING QSORT ROUTINE **/
    qsortr2( n, ind, tbl1, tbl2 ) ;
        
                         /** CONVERT FROM C SUBSCRIPTS BACK TO FORTRAN SUBS **/
    for ( i = 0 ; i < n ; i++ )
        {
        ind[ i ]++ ;
        }

    return ;

    } /*********************************************  END FUNCTION SORTR2() **/

void SORTR3( const FINT* nelts,   /** number of elements                     **/
             FINT        ind[],   /** index-array                            **/
             const FREAL tbl1[],  /** table:  first  key-component in tuple  **/
             const FREAL tbl2[],  /** table:  second key-component in tuple  **/
             const FREAL tbl3[] ) /** table:  third  key-component in tuple  **/
    {
    int n, i ;

    n = *nelts ;

                              /** CONVERT FROM FORTRAN SUBSCRIPTS TO C SUBS **/
    for ( i = 0 ; i < n ; i++ )
        {
        ind[ i ]-- ;
        } ;

                                           /** CALL C-BINDING QSORT ROUTINE **/
    qsortr3( n, ind, tbl1, tbl2, tbl3 ) ;
        
                         /** CONVERT FROM C SUBSCRIPTS BACK TO FORTRAN SUBS **/
    for ( i = 0 ; i < n ; i++ )
        {
        ind[ i ]++ ;
        }

    return ;

    } /*********************************************  END FUNCTION SORTR3() **/


void SORTR4( const FINT* nelts,    /** number of elements                     **/
             FINT        ind[],    /** index-array                            **/
             const FREAL tbl1[],   /** table:  first  key-component in tuple  **/
             const FREAL tbl2[],   /** table:  second key-component in tuple  **/
             const FREAL tbl3[],   /** table:  third  key-component in tuple  **/
             const FREAL tbl4[] )  /** table:  fourth key-component in tuple  **/
    {
    FINT  n, i ;

    n = *nelts ;

                              /** CONVERT FROM FORTRAN SUBSCRIPTS TO C SUBS **/
    for ( i = 0 ; i < n ; i++ )
        {
        ind[ i ]-- ;
        } ;

                                           /** CALL C-BINDING QSORT ROUTINE **/
    qsortr4( n, ind, tbl1, tbl2, tbl3, tbl4 ) ;
        
                         /** CONVERT FROM C SUBSCRIPTS BACK TO FORTRAN SUBS **/
    for ( i = 0 ; i < n ; i++ )
        {
        ind[ i ]++ ;
        }

    return ;

    } /*********************************************  END FUNCTION SORTR4() **/

