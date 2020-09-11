/**************************************************************************
VERSION "$Id: dscgridc.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    reads specified layer of variable "cname" from file with 
    logical name "gname" and puts it into "buffer".
    C wrapper around I/O API Fortran binding routine DSCGRID().


PRECONDITIONS:
    gname already opened by OPEN3() or open3c()
    cname a valid variable in gname, or else is ALLVAR3=='ALL'

CALLS:
    I/O API's Fortran-binding DSCGRID()

REVISION HISTORY:
    Prototype 3/1995 by CJC

    Version   8/1999 by CJC for I/O APIv2:  WIN32 stuff

    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
**************************************************************************/

#include  <string.h>
#include  "iodecl3.h"


		/** HACKS FOR FELDMAN-DESCENDED F77'S FOLLOW: **/

#if FLDMN

#define DSCGRID dscgrid_
#define DSCOORD dscoord_

#elif defined(__hpux) || defined(_AIX)

#define DSCGRID dscgrid
#define DSCOORD dscoord

#endif


#if defined(DSCGRID) || defined(ABSFT)

    extern FINT DSCGRID( const char * gname ,
                         char       * cname ,
                         FINT       * ctype ,
                         double     * p_alp ,
                         double     * p_bet ,
                         double     * p_gam ,
                         double     * xcent ,
                         double     * ycent ,
                         double     * xorig ,
                         double     * yorig ,
                         double     * xcell ,
                         double     * ycell ,
                         FINT       * ncols ,
                         FINT       * nrows ,
                         FINT       * nthik ,
                         FSTR_L       gnamlen ,
                         FSTR_L       cnamlen ) ;

    extern FINT DSCOORD( const char * cname ,
                         FINT       * ctype ,
                         double     * p_alp ,
                         double     * p_bet ,
                         double     * p_gam ,
                         double     * xcent ,
                         double     * ycent ,
                         FSTR_L       cnamlen ) ;

int dscgridc( const char * gname ,
              char       * cname ,
              int        * ctype ,
              double     * p_alp ,
              double     * p_bet ,
              double     * p_gam ,
              double     * xcent ,
              double     * ycent ,
              double     * xorig ,
              double     * yorig ,
              double     * xcell ,
              double     * ycell ,
              int        * ncols ,
              int        * nrows ,
              int        * nthik )

    {       /*  begin body of dscgridc() */
    FINT  type, cols, rows, thik ;

    type = (FINT) *ctype ;
    cols = (FINT) *ncols ;
    rows = (FINT) *nrows ;
    thik = (FINT) *nthik ;

    return DSCGRID( gname , 
                    cname , 
                   & type ,
                    p_alp ,
                    p_bet ,
                    p_gam ,
                    xcent ,
                    ycent ,
                    xorig ,
                    yorig ,
                    xcell ,
                    ycell ,
                   & cols ,
                   & rows ,
                   & thik ,
                    STRLEN( gname ) , 
                    ( FSTR_L) NAMLEN3 ) ;

    }       /*  end body of dscgridc ()  */

int dscoordc( const char * cname ,
              int        * ctype ,
              double     * p_alp ,
              double     * p_bet ,
              double     * p_gam ,
              double     * xcent ,
              double     * ycent )

    {       /*  begin body of dscgridc() */
    FINT  type ;

    type = (FINT) *ctype ;

    return DSCOORD( cname , 
                   & type ,
                    p_alp ,
                    p_bet ,
                    p_gam ,
                    xcent ,
                    ycent ,
                    STRLEN( cname ) ) ;

    }       /*  end body of dscoordc ()  */

                	/** END  CASE OF FELDMAN-DESCENDED F77 TARGETS **/
                	/** NEXT CASE:  CRAY CF77-TARGETED dscgridc(): **/


#elif defined(_WIN32)

    extern int DSCGRID( const char * gname ,
                        FSTR_L       gnamlen ,
                        char       * cname ,
                        FSTR_L       cnamlen ,
                        FINT       * ctype ,
                        double     * p_alp ,
                        double     * p_bet ,
                        double     * p_gam ,
                        double     * xcent ,
                        double     * ycent ,
                        double     * xorig ,
                        double     * yorig ,
                        double     * xcell ,
                        double     * ycell ,
                        FINT       * ncols ,
                        FINT       * nrows ,
                        FINT       * nthik );

    extern int DSCOORD( const char * cname ,
                        FSTR_L       cnamlen ,
                        FINT       * ctype ,
                        double     * p_alp ,
                        double     * p_bet ,
                        double     * p_gam ,
                        double     * xcent ,
                        double     * ycent );

int dscgridc( const char * gname ,
              char       * cname ,
              int        * ctype ,
              double     * p_alp ,
              double     * p_bet ,
              double     * p_gam ,
              double     * xcent ,
              double     * ycent ,
              double     * xorig ,
              double     * yorig ,
              double     * xcell ,
              double     * ycell ,
              int        * ncols ,
              int        * nrows ,
              int        * nthik )

    {       /*  begin body of dscgridc() */
    FINT  type, cols, rows, thik ;

    type = (FINT) *ctype ;
    cols = (FINT) *ncols ;
    rows = (FINT) *nrows ;
    thik = (FINT) *nthik ;

    return DSCGRID( gname , 
                    STRLEN( gname ) , 
                    cname , 
                    NAMLEN3 ,
                   & type ,
                    p_alp ,
                    p_bet ,
                    p_gam ,
                    xcent ,
                    ycent ,
                    xorig ,
                    yorig ,
                    xcell ,
                    ycell ,
                   & cols ,
                   & rows ,
                   & thik );

    }       /*  end body of dscgridc ()  */


int dscoordc( const char * cname ,
              int        * ctype ,
              double     * p_alp ,
              double     * p_bet ,
              double     * p_gam ,
              double     * xcent ,
              double     * ycent )

    {       /*  begin body of dscoordc() */
    FINT  type ;

    type = (FINT) *ctype ;

    return DSCOORD( cname , 
                    STRLEN( cname )
                   & type ,
                    p_alp ,
                    p_bet ,
                    p_gam ,
                    xcent ,
                    ycent  ) ;

    }       /*  end body of dscoordc ()  */


#elif  defined(_CRAY)

#include <fortran.h>

    extern int DSCGRID( const _fcd   gname ,
                        _fcd         cname ,
                        FINT       * ctype ,
                        double     * p_alp ,
                        double     * p_bet ,
                        double     * p_gam ,
                        double     * xcent ,
                        double     * ycent ,
                        double     * xorig ,
                        double     * yorig ,
                        double     * xcell ,
                        double     * ycell ,
                        FINT       * ncols ,
                        FINT       * nrows ,
                        FINT       * nthik ) ;

    extern int DSCOORD( const _fcd   cname ,
                        int        * ctype ,
                        double     * p_alp ,
                        double     * p_bet ,
                        double     * p_gam ,
                        double     * xcent ,
                        double     * ycent ) ;

int dscgridc( const char * gname ,
              char       * cname ,
              int        * ctype ,
              double     * p_alp ,
              double     * p_bet ,
              double     * p_gam ,
              double     * xcent ,
              double     * ycent ,
              double     * xorig ,
              double     * yorig ,
              double     * xcell ,
              double     * ycell ,
              int        * ncols ,
              int        * nrows ,
              int        * nthik )

    {       /*  begin body of dscgridc() */

    _fcd  grid;
    _fcd  coord;
    
    FINT  type, cols, rows, thik ;

    type = (FINT) *ctype ;
    cols = (FINT) *ncols ;
    rows = (FINT) *nrows ;
    thik = (FINT) *nthik ;

    grid  = _cptofcd( (char *)gname, STRLEN( gname ) ) ;
    coord = _cptofcd( (char *)cname, NAMLEN3 ) ;

    return _btol( DSCGRID( grid  ,
                           coord , 
                          & type ,
                           p_alp ,
                           p_bet ,
                           p_gam ,
                           xcent ,
                           ycent ,
                           xorig ,
                           yorig ,
                           xcell ,
                           ycell ,
                          & cols ,
                          & rows ,
                          & thik ) ) ;
                     
    }       /*  end body of dscgridc ()  */

int dscoordc( const char * cname ,
              int        * ctype ,
              double     * p_alp ,
              double     * p_bet ,
              double     * p_gam ,
              double     * xcent ,
              double     * ycent )

    {       /*  begin body of dscgridc() */

    _fcd  coord;
    
    FINT  type ;

    type  = (FINT) *ctype ;
    coord = _cptofcd( (char *)cname, STRLEN( cname ) ) ;

    return _btol( DSCOORD( coord, 
                          & type ,
                           p_alp ,
                           p_bet ,
                           p_gam ,
                           xcent ,
                           ycent  ) ) ;
                     
    }       /*  end body of dscoordc ()  */

                	/** END  CASE OF CRAY CF77-TARGETED dscgridc(): **/

#else

#error   "Error compiling dscgridc()/dscoordc():  unsupported architecture"

#endif              /** #IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

