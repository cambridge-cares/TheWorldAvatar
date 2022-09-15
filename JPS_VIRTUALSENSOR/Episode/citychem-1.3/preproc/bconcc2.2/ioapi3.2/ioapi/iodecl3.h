
/************************************************************************
INCLUDE FILE  iodecl3.h

    Version "$Id: iodecl3.h 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.
    Copyright (C) 1992-2002 MCNC,
    (C) 1992-2002,2005-2013  Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron AAdvanced Meteorological Systems, LLC (BAMS)
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

    DO NOT EDIT !!

        The EDSS/Models-3 I/O API depends in an essential manner
        upon the contents of this INCLUDE file.  ANY CHANGES are
        likely to result in very obscure, difficult-to-diagnose
        bugs caused by an inconsistency between standard "libioapi.a"
        object-libraries and whatever code is compiled with the
        resulting modified INCLUDE-file.

        By making any changes to this INCLUDE file, the user
        explicitly agrees that in the case any assistance is 
        required of MCNC or of the I/O API author, CARLIE J. COATS, JR.
        HE AND/OR HIS PROJECT OR CONTRACT AGREES TO REIMBURSE MCNC
        AND/OR THE I/O API AUTHOR, CARLIE J. COATS, JR., AT A
        RATE TRIPLE THE NORMAL CONTRACT RATE FOR THE SERVICES
        REQUIRED.

CONTAINS:  
       Typedefs and prototypes of C wrappers for I/O API

DEPENDENT UPON:  
       consistency with FORTRAN include-file IODECL3.EXT, FDESC3.EXT

REVISION HISTORY:  
    prototype 5/9/95 by CJC

    Modified  8/99 by CJC:  FLDMN, Win32 portability enhancements

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.

**************************************************************************/

#ifndef    IODECL3_DEFINED
#define    IODECL3_DEFINED


#ifdef __cplusplus
extern "C" {
#endif


#include   "parms3.h"  /*  defines FLDMN and FREAL */
#include   "fdesc3.h"

/*************************************************************************      
        Prototype declarations for public C bindings to I/O API
        In the following: 
            "fname" and "vname" are character strings of length 
            at most 16=NAMLEN3, for the file and variable names 
            respectively.
            "jdate" and "jtime" are date and time, encoded as 
            integers in the formats YYYYDDD and HHMMSS, respectively.
            "buffer" is the array (or other data structure) for the
            data being read or written.
            
**************************************************************************/

#if FLDMN || defined(_WIN32)

#define GCD              gcd_ 
#define GETDTTIME        getdttime_
#define IOPARMS3         ioparms3_ 
#define JSTEP3           jstep3_ 
#define POLY             poly_
#define BDESC3           bdesc3_
#define CDESC3           cdesc3_

#elif defined(__hpux) || defined(_AIX)

#define GCD              gcd
#define GETDTTIME        getdttime
#define IOPARMS3         ioparms3
#define JSTEP3           jstep3
#define POLY             poly
#define BDESC3           bdesc3
#define CDESC3           cdesc3

#elif  defined(ABSFT)

#define BDESC3           _CBDESC3
#define CDESC3           _CCDESC3

#elif  defined(_CRAY)

#else
 
#error   "Error compiling:  unsupported architecture"
 
#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/



                                        /** I/O API file description struct **/
extern IOAPI_Bdesc3  BDESC3 ;
extern IOAPI_Cdesc3  CDESC3 ;


                                        /** I/O API functions **/

int init3c(  void ) ;                   /** start up I/O API **/

int shut3c(  void ) ;                   /** shut down I/O API **/

int open3c(  const char  * fname  ,
             const IOAPI_Bdesc3  * bdesc ,
             const IOAPI_Cdesc3  * cdesc ,
             int           status ,
             const char  * pname  ) ;   /** open file fname **/

int check3c( const char  * fname ,
             const char  * vname ,
             int           jdate ,
             int           jtime ) ;    /** check if vble OK for date&time **/

int close3c( const char  * fname ) ;    /** close file fname   **/

int desc3c(  const char    * fname ,
             IOAPI_Bdesc3  * bdesc ,
             IOAPI_Cdesc3  * cdesc  ) ; /** get file description for fname **/

int sync3c( const char  * fname ) ;    /** disk-synch for file fname   **/

int read3c(  const char  * fname ,
             const char  * vname ,
             int           layer ,
             int           jdate ,
             int           jtime ,
             void        * buffer ) ;   /** read vble(s) @ date&time  **/

int write3c( const char  * fname ,
             const char  * vname ,
             int           jdate ,
             int           jtime ,
             const void  * buffer ) ;   /** write vble(s) @ date&time **/

int xtract3c( const char  * fname , const char  * vname ,
              int           lolay , int           hilay ,
              int           lorow , int           hirow ,
              int           locol , int           hicol ,
              int           jdate , int           jtime ,
              void        * buffer ) ; /** read subwindow of grd vble **/

int interp3c( const char  * fname ,    /*   file  name  */
              const char  * vname ,    /*   vble  name  */
              const char  * cname ,    /*  caller name  */
              int           jdate ,
              int           jtime ,
              int           bsize ,
              FREAL       * buffer ) ;  /** time interp of grd|bdy vble **/

int interp3x( const char  * fname ,    /*   file  name   */
              const char  * vname ,    /*   vble  name   */
              const char  * cname ,    /*  caller name   */
              int           col0  ,    /*  window bounds */
              int           col1  ,
              int           row0  ,
              int           row1  ,
              int           lay0  ,
              int           lay1  ,
              int           jdate ,
              int           jtime ,
              FREAL       * buffer ) ;  /** time interp of grd|bdy vble **/

int ddtvar3c( const char  * fname ,    /*   file  name  */
              const char  * vname ,    /*   vble  name  */
              const char  * cname ,    /*  caller name  */
              int           jdate ,
              int           jtime ,
              int           bsize ,
              FREAL       * buffer ) ;  /** time-derivative of grd|bdy vble **/

int filchk3c( const char * fname ,
              const int    ftype ,
              const int    ncols ,
              const int    nrows ,
              const int    nlays ,
              const int    nthik ) ;

                                        /** UTILITY functions: **/

int currstepc( int   jdate , int   jtime ,
               int   sdate , int   stime , int   tstep,
               int * cdate , int * ctime ) ; 
                                      /* timestep enclosing jdate:jtime */

void   daymonc( int   jdate ,
                int * month ,
                int * mday ) ;   /** month and day-of-month for jdate **/

int dscoordc( const char * cname ,
              int        * ctype ,
              double     * p_alp ,
              double     * p_bet ,
              double     * p_gam ,
              double     * xcent ,
              double     * ycent ) ;  /** get coord sys description **/

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
              int        * nthik ) ;  /** get grid description **/

void   dt2strc( int   jdate ,
                int   jtime ,
                char  buffer[ 25 ] ) ;  /** date-and-time to string **/

int envync( const char * lname       , 
            const char * description , 
            int          defaultval  ,
            int        * status );  /** Get Y-N environment-vble value **/

int envintc( const char * lname       ,
             const char * description ,
             int          defaultval  ,
             int        * status  ); /** Get int environment-vble value **/

float envrealc( const char * lname       , 
                const char * description , 
                float        defaultval  ,
                int        * status ); /** Get float env-vble value **/

double envdblec( const char * lname       , 
                 const char * description , 
                 double       defaultval  ,
                 int        * status ); /** Get double env-vble value **/

void envstrc( const char * lname       , 
              const char * description , 
              const char * defaultval  ,
              char       * evalue      ,
              int        * status      ,
              int          elen ) ; /** get string env-vble value  **/

int find1c( int        k1,
            int        n,
            const int *list1 ); /** look up integer in sorted key table **/

int find2c( int        k1,
            int        k2,
            int        n, 
            const int *list1 ,
            const int *list2 ) ; /** look up <K1,K2> in 2-key table **/

int find3c( int        k1,
            int        k2,
            int        k3,
            int        n, 
            const int *list1 ,
            const int *list2 ,
            const int *list3 ) ; /** look up <K1,K2,K3> in 3-key table **/

int find4c( int        k1,
            int        k2,
            int        k3,
            int        k4,
            int        n, 
            const int *list1 ,
            const int *list2 ,
            const int *list3 ,
            const int *list4 ) ; /* look up <K1,K2,K3,K4> in 4-key table */
 
int GCD( const int *p, const int *q ) ;  /** greatest common divisor **/

int getdfilec( const char          * fname  ,
               int                   rstatus,
               int                   fstatus,
               int                   reclen,
               const char          * pname  ) ; /** Open F. dir-acc file **/

int getefilec( const char          * fname  ,
               int                   rstatus,
               int                   fstatus,
               const char          * pname  ) ; /** Open F. seq file **/

void GETDTTIME( int * now_date, 
                int * now_time ) ;      /** Current wall-clock time **/

void   hhmmssc( int   jtime ,
                char  buffer[ 11 ] ) ;  /** time to string "hh:mm"ss" **/       

void IOPARMS3( int * mxdlen, 
               int * namlen, 
               int * mxfile, 
               int * mxvars, 
               int * mxdesc, 
               int * mxlays, 
               int * mxatts ) ;  /** return I/O API dimensioning parameters **/  

int  JSTEP3( const int * jdate, 
             const int * jtime, 
             const int * sdate,     /** record number 1, ...  in timestep **/
             const int * stime,     /** sequence, or (-1) if jdate:jtime  **/
             const int * tstep ) ;  /** not on the sequence               **/

int    julianc( int   year  ,
                int   month ,
                int   mday ) ;    /** day 1...365,6 for indicated date **/

void m3errc( const char * caller ,
             int          jdate ,
             int          jtime ,    /** error/warning message with        **/
             const char * errtxt ,   /** optional shutdown of I/O API and  **/
             int          fatal ) ;  /** program termination by exit( 2 )  **/

void m3exitc( const char * caller ,
              int          jdate  ,
              int          jtime  ,    /** error message with shutdown of   **/
              const char * errtxt ,    /** I/O API and program termination  **/
              int          errstat ) ; /** by exit( errstat )               **/

void m3mesgc( const char * mesg ) ;   /** write mesg to the Fortran-pgm log **/

void m3warnc( const char * caller ,   /** Warning mesg to Fortran-pgm log **/
              int          jdate ,
              int          jtime ,
              const char * errtxt );

void   mmddyyc( int   jdate ,
                char  buffer[ 15 ] ) ;  /** date to string "Month DD, YYYY" **/

void  nextimec( int * jdate ,
                int * jtime ,
                int   tstep ) ;  /** Update jdate:jtime by tstep **/
                
float POLY( const float *x,
            const float *xpts,
            const float *ypts,
            const int   *deg ) ;  /** polynomial interpolation **/

void qsorti1( int        n,          /** number of elements             **/
              int        ind[],      /** index-array                    **/
              const int  tbl1[] ) ;  /** first  key-component in tuple  **/

void qsorti2( int        n,           /** number of elements             **/
              int        ind[],       /** index-array                    **/
              const int  tbl1[],      /** first  key-component in tuple  **/
              const int  tbl2[] ) ;   /** second key-component in tuple  **/

void qsorti3( int        n,           /** number of elements             **/
              int        ind[],       /** index-array                    **/
              const int  tbl1[],      /** first  key-component in tuple  **/
              const int  tbl2[],      /** second key-component in tuple  **/
              const int  tbl3[] ) ;   /** third  key-component in tuple  **/

void qsorti4( int        n,           /** number of elements             **/
              int        ind[],       /** index-array                    **/
              const int  tbl1[],      /** first  key-component in tuple  **/
              const int  tbl2[],      /** second key-component in tuple  **/
              const int  tbl3[],      /** third  key-component in tuple  **/
              const int  tbl4[] ) ;   /** fourth key-component in tuple  **/

int  sec2timec( int  seconds ) ;  /** convert seconds to HHMMSS **/

int  secsdiffc( int  adate ,
                int  atime ,
                int  zdate ,
                int  ztime ) ;    /** time difference from a* to z*  **/

int  time2secc( int  atime ) ;   /** HHMMSS ~~> seconds **/
               
int     wkdayc( int  jdate ) ;   /** day of week 1...7 for jdate **/


#if FLDMN || defined(__hpux) || defined(_AIX) || defined(ABSFT)

void  name2cstr( const char * source, 
                 char       * target,
                 FSTR_L       slen,
                 FSTR_L       tlen ) ;

void  fstr2cstr( const char * source, 
                 char       * target, 
                 FSTR_L       slen, 
                 FSTR_L       tlen ) ;

void  cstr2fstr( const char * source, 
                 char *       target, 
                 FSTR_L       tlen ) ;

#elif  defined(_WIN32)

void  name2cstr( const char * source, 
                 char       * target,
                 FSTR_L       slen,
                 FSTR_L       tlen ) ;

void  fstr2cstr( const char * source, 
                 char       * target, 
                 FSTR_L       slen, 
                 FSTR_L       tlen ) ;

void  cstr2fstr( const char * source, 
                 char *       target, 
                 FSTR_L       tlen ) 

#elif  defined(_CRAY)

#include <fortran.h>

void  name2cstr( const _fcd   source, 
                 char       * target,
                 FSTR_L       tlen  ) ;

void  fstr2cstr( const _fcd   source, 
                 char       * target, 
                 FSTR_L       tlen ) ;

void  cstr2fstr( const char * source,
                 _fcd         target ) ;

#endif   /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF WIN32, OR IF CRAY **/

#ifdef __cplusplus
}
#endif


#endif    /*   IODECL3_DEFINED  */

/****************   END   iodecl3.h   ***********************************/


