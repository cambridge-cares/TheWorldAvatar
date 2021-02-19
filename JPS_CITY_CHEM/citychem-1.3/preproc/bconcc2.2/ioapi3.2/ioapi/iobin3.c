
/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
VERSION:
    EDSS/Models-3 I/O API -- Version 3
    "iobin3.c" version "$Id: iobin3.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems, and
    (C) 2015 UNC Institute for the Environment.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
    I/O API PRIVATE data structures and Fortran callable
    lower-layer routines for the BINIO3  implementation layer
    of the Models-3 I/O API.

    These routines are all INTEGER functions that return nonzero
    for success (usually, 1), zero for failure.

INTEGER     INITBIN3()   - Initialize internal data structures
INTEGER     CRTBIN3()    - Create a new BINIO3 file, and write its file header
INTEGER     OPNBIN3()    - Open an existing BINIO3 file
INTEGER     RDBFLAG()    - read  a specified time step flag
INTEGER     WRBFLAG()    - write a specified time step flag
INTEGER     RDBVARS()    - read  a specified timestep/variable record     
INTEGER     WRBVARS()    - write a specified timestep/variable record 
INTEGER     XTRBIN3()    - extract a "window" from a GRIDDED BINIO3 file
INTEGER     FLUSHBIN3()  - flush a specified BINIO3 file to disk
INTEGER     CLOSEBIN3()  - close a specified BINIO3 file
INTEGER     WRBPATCH()   - write a specified slab timestep/variable record
                           for distributed version of EPA's CMAQ

CALLS:  none

REVISION HISTORY:
    Prototype 10/2003 by CJC for I/O APIv3

    Bug-fixes 12/2003-1/2004 from Jeffrey O. Young and David Wong (US EPA)

    Modified 12/2004:  NVARS range check in OPNBIN3

    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.

    Modified 02/2015 by CJC for I/O API version 3.2:  M3INT8 (INTEGER*8) support

BINFIL3 file Structure:

        Fixed header:  size  sizeof( Bin_Hdr3 )
        --first 4 bytes contains (1-based, Fortran style)
          current maximum timestep-record number, or 0
        --size  sizeof( Bin_Hdr3 )

        Array [ nvars variables ] of variables-names:
        --size  nvars*sizeof( M3Name )

        Array [ nvars variables ] of variables-units:
        --size  nvars*sizeof( M3Name )

        Array [ nvars variables ] of variables-descriptions:
        --size  nvars*sizeof( M3Line )

        Sequence of timestep records:
        --sizes  fstate[f]->trecsize
        --starting at  sizeof( Bin_Hdr3 )
                       + nvars * ( 2*sizeof( M3Name ) + sizeof( M3LINE ) )
                       + ( step - 1 )*fstate[f]->trecsize

            array [ variables v ][2] of INT4 timestep flags:
            --size  2*nvars*sizeof( INT4 )

            array[ variables v ] of records:
            -- sizes  fstate[f]->vrecsiz[v]
            -- starting at <start of timestep> + fstate[f]->voffset[v]
        
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- */

#include <unistd.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>

#include "iodecl3.h"
#include "state3.h"

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

#define BDESC3     bdesc3_
#define CDESC3     cdesc3_
#define BSTATE3    bstate3_
#define CSTATE3    cstate3_

#define INITBIN3   initbin3_
#define CRTBIN3    crtbin3_
#define OPNBIN3    opnbin3_
#define DSCBIN3    dscbin3_
#define RDBFLAG    rdbflag_
#define WRBFLAG    wrbflag_
#define RDBVARS    rdbvars_
#define WRBVARS    wrbvars_
#define XTRBIN3    xtrbin3_
#define FLUSHBIN3  flushbin3_
#define CLOSEBIN3  closebin3_
#define WRBPATCH   wrbpatch_

#elif defined(__hpux) || defined(_AIX)

/** Hack for no-underscore quasi-Feldman-descended f77's follows: **/

#define BDESC3     bdesc3
#define CDESC3     cdesc3
#define BSTATE3    bstate3
#define CSTATE3    cstate3

#define INITBIN3   initbin3
#define CRTBIN3    crtbin3
#define OPNBIN3    opnbin3
#define DSCBIN3    dscbin3
#define RDBFLAG    rdbflag
#define WRBFLAG    wrbflag
#define RDBVARS    rdbvars
#define WRBVARS    wrbvars
#define XTRBIN3    xtrbin3
#define FLUSHBIN3  flushbin3
#define CLOSEBIN3  closebin3
#define WRBPATCH   wrbpatch

#elif defined(ABSFT)

#define BDESC3     _CBDESC3
#define CDESC3     _CCDESC3
#define BSTATE3    _CBSTATE3
#define CSTATE3    _CCSTATE3

#else

#error  "Error compiling iobin.c:  unsupported architecture"

#endif

/** -------------------------------------------------------------- **/
/*  Macros to extract least-significant-byte, next byte, ...  */

#define BYTE0( i ) ((unsigned char)   (i)              & 255 )
#define BYTE1( i ) ((unsigned char) ( (i) /      256 ) & 255 )
#define BYTE2( i ) ((unsigned char) ( (i) /    65536 ) & 255 )
#define BYTE3( i ) ((unsigned char) ( (i) / 16777216 ) & 255 )

#define HDR_MODE        ((int) 1)
#define  RW_MODE        ((int) 2)


/********************  Data Type Definitions  ****************************/


typedef unsigned char INT4 [ 4] ;     /** header representation for "int32" **/
typedef char          HREAL[16] ;     /**  " for Fortran REAL ( 1PE15.8\0)  **/
typedef char          HDBLE[28] ;     /**  " for Fortran DBLE (1PE27.19\0)  **/

                                /** start of binary image of a file header **/ 
                                /** followed by  NVARS M3Name vble-names  **/
                                /**     ...then  NVARS M3Name vble-units  **/
                                /**     ...then  NVARS M3Line vble-descs  **/
typedef struct{
              INT4      nrecs ;         /**  at seek offset 0:  note that   **/
              M3Name    ioapi_vrsn ;    /**  nrecs is rewritten repeatedly  **/
              INT4      byte_order ;
              INT4      intsize ;
              INT4      rsize ;
              INT4      dsize ;
              INT4      cdate ;
              INT4      ctime ;
              INT4      wdate ;
              INT4      wtime ;
              INT4      ftype ;
              INT4      gdtyp ;
              INT4      vgtyp ;
              INT4      ncols ;
              INT4      nrows ;
              INT4      nlays ;
              INT4      nthik ;
              INT4      nvars ;
              INT4      sdate ;
              INT4      stime ;
              INT4      tstep ;
              HDBLE     p_alpha ;
              HDBLE     p_beta  ;
              HDBLE     p_gamma ;
              HDBLE     x_center  ;
              HDBLE     y_center  ;
              HDBLE     x_origin  ;
              HDBLE     y_origin  ;
              HDBLE     x_cellsize  ;
              HDBLE     y_cellsize  ;
              HREAL     vgtop  ;
              HREAL     vglvl[ MXLAYS3+1]  ;
              INT4      vtype[ MXVARS3 ] ;
              M3Name    gridname ;
              M3Name    updtname ;
              M3Line    execution ;
              M3Line    file_desc[ MXDESC3 ] ;
              M3Line    updt_desc[ MXDESC3 ] ; }  Bin_Hdr3 ;

                                /** per-file functional state metadata **/ 
typedef struct{
              off_t    hdrsize ;
              off_t    trecsize ;
              off_t    vrecsiz[ MXVARS3 ] ;     /*  size:  one layer of one vble  */
              off_t    voffset[ MXVARS3 ] ;     /*  offset rel start of timestep  */
              size_t   isize ;
              size_t   rsize ;
              size_t   dsize ;
              FILE   * fptr ;
              int      fmode ;
              int      ftype ;
              int      ncols ;
              int      nrows ;
              int      nlays ;
              int      nthik ;
              int      nvars ;
              int      nrecs ;
              int      vtype[ MXVARS3 ] ; }  Bin_State3 ;

/********************  Data Structures  **********************************/

extern IOAPI_Bdesc3      BDESC3 ;
extern IOAPI_Cdesc3      CDESC3 ;

extern IOAPI_BSTATE3     BSTATE3 ;
extern IOAPI_CSTATE3     CSTATE3 ;

static M3Name            ioapi_version ;
static int               byte_order ;
static int               run_date ;
static int               run_time ;

static Bin_State3 * fstate[ MXFILE3+1 ] ;

/*  type sizes, subscripted by netCDF type number  */

static size_t   tsizes[] = { 0 , 
                             1, 
                             1,
                             sizeof( short int ),
                             sizeof( FINT ) ,
                             sizeof( FREAL ) ,
                             sizeof( double ) } ;

static char   *fmodes[ 2 ] = { "r",         /*  NF_NOWRITE = 0  */
                               "r+"         /*  NF_WRITE   = 1  */
                               } ;

/********************  Worker Routines  **********************************/

static void set_int4( INT4 int4 , int i )
    {
    int4[0] = BYTE0( i ) ;
    int4[1] = BYTE1( i ) ;
    int4[2] = BYTE2( i ) ;
    int4[3] = BYTE3( i ) ;
    }                   /** END   static void set_int4() **/

static int get_int4( INT4 int4 )
    {
    return (          (int)int4[0]
           +      256*(int)int4[1]
           +    65536*(int)int4[2]
           + 16777216*(int)int4[3] ) ;
    }                   /** END   static int get_int4() **/

static int  set_fstate( int f,
                        int ncol,
                        int nrow,
                        int nlay,
                        int nthk,
                        int nvar,
                        int nrec,
                        int ftyp,
                        int gtyp,
                        int vtyp[],
                        int mode,
                        int isiz,
                        int fsiz,
                        int dsiz )
    {
    off_t       rsize, vsize, ssum ;
    int         v, result ;
    char        mesg[ 80 ] ;

    result = 1 ;                /**  presumed success  **/

    BSTATE3.nvars[ f ] = nvar ;
    BSTATE3.nlays[ f ] = nlay ;
    BSTATE3.nrows[ f ] = nrow ;
    BSTATE3.ncols[ f ] = ncol ;
    BSTATE3.nthik[ f ] = nthk ;
    BSTATE3.mxrec[ f ] = nrec ;
    BSTATE3.ftype[ f ] = ftyp ;
    BSTATE3.gdtyp[ f ] = gtyp ;

    fstate[ f ]->nrecs = nrec ;
    fstate[ f ]->fmode = mode ;
    fstate[ f ]->ftype = ftyp ;
    fstate[ f ]->ncols = ncol ;
    fstate[ f ]->nrows = nrow ;
    fstate[ f ]->nlays = nlay ;
    fstate[ f ]->nvars = nvar ;
    fstate[ f ]->nthik = nthk ;
    fstate[ f ]->isize = isiz ;
    fstate[ f ]->rsize = fsiz ;
    fstate[ f ]->dsize = dsiz ;

   if ( nvar == 0 )
       {
       ssum = 2 * sizeof( INT4 ) ;
       fstate[ f ]->voffset[ 0 ] = ssum ;
       }
   else{
       ssum = 2 * nvar * sizeof( INT4 ) ;
       }

    if ( ftyp == CUSTOM3 )
        {
        rsize = (off_t) ncol ;
        }
    else if ( ftyp == GRDDED3 )
        {
        rsize = (size_t)( ncol * nrow ) ;
        }
    else if ( ftyp == BNDARY3 )
        {
        rsize = 2 * ( nthk > 0 ? nthk : -nthk ) ;
        rsize = (size_t)( rsize * ( ncol +  nrow + 2 * nthk ) ) ;
        }
    else if ( ftyp == IDDATA3 )   /*  ALLVARS-only data types first  */
        {
        ssum += (off_t)( isiz * ( 1 + nrow ) ) ;
        rsize = (off_t) nrow ;
        }
    else if ( ftyp == PROFIL3 )
        {
        ssum += (off_t)( isiz * ( 1  + 2 * nrow ) 
                       + dsiz * nrow * 3 ) ;
        rsize = (off_t) ( ncol * nrow ) ;
        }
    else if ( ftyp == SMATRX3 )
        {
        ssum += (off_t)( isiz * ( ncol + nrow ) ) ;
        rsize = (size_t)( ncol ) ;
        }
    else{
        sprintf( mesg, "CRTBIN3():  unsupported FTYPE %d", ftyp  ) ;
        m3mesgc( mesg ) ;
        return( (FINT) 0 ) ;
        } ;

    BSTATE3.bsize[ f ] = (FINT) rsize ;
    
    for ( v = 0 ; v < nvar ; v++ )
        {
        fstate[ f ]->vtype[ v ] = vtyp[ v ] ;
        BSTATE3.vtype[ f ][ v ] = vtyp[ v ] ;

        if ( vtyp[ v ] == M3INT )
            {
            vsize = rsize * (off_t)isiz ;
            }
        else if ( vtyp[ v ] == M3REAL )
            {
            vsize = rsize * (off_t)fsiz ;
            }
        else if ( vtyp[ v ] == M3DBLE )
            {
            vsize = rsize * (off_t)dsiz ;
            }
        else if ( vtyp[ v ] == M3INT8 )
            {
            vsize = rsize * (off_t)dsiz ;
            }
        else{
            result = 0 ;
            sprintf( mesg, "Variable %d:  invalid type %d", v, vtyp[ v ] ) ;
            m3mesgc( mesg ) ;
            }

        fstate[ f ]->vrecsiz[ v ] = vsize ;
        fstate[ f ]->voffset[ v ] = ssum ;
        ssum += ( vsize * nlay );
        }

    fstate[ f ]->trecsize = ssum ;

    fstate[ f ]->hdrsize  = (off_t) ( sizeof( Bin_Hdr3 ) 
                                    + nvar * sizeof( M3Name )
                                    + nvar * sizeof( M3Name )
                                    + nvar * sizeof( M3Line ) ) ;

#ifdef  BIN3_DEBUG
    m3mesgc( " " ) ;
    sprintf( mesg, "IOBIN3/set_fstate(%d)", f ) ;
    m3mesgc( mesg ) ;
    
    sprintf( mesg, "fstate[%d]->nrecs = %d", f, fstate[ f ]->nrecs ) ;
    m3mesgc( mesg ) ;
    sprintf( mesg, "fstate[%d]->fmode = %d", f, fstate[ f ]->fmode ) ;
    m3mesgc( mesg ) ;
    sprintf( mesg, "fstate[%d]->ftype = %d", f, fstate[ f ]->ftype ) ;
    m3mesgc( mesg ) ;
    sprintf( mesg, "fstate[%d]->ncols = %d", f, fstate[ f ]->ncols ) ;
    m3mesgc( mesg ) ;
    sprintf( mesg, "fstate[%d]->nrows = %d", f, fstate[ f ]->nrows ) ;
    m3mesgc( mesg ) ;
    sprintf( mesg, "fstate[%d]->nlays = %d", f, fstate[ f ]->nlays ) ;
    m3mesgc( mesg ) ;
    sprintf( mesg, "fstate[%d]->nvars = %d", f, fstate[ f ]->nvars ) ;
    m3mesgc( mesg ) ;
    sprintf( mesg, "fstate[%d]->nthik = %d", f, fstate[ f ]->nthik ) ;
    m3mesgc( mesg ) ;
    for ( v = 0 ; v < nvar ; v++ )
        {
        sprintf( mesg, "fstate[%d]->vrecsiz[ %d ] = %d", f, v, (int)fstate[ f ]->vrecsiz[ v ] ) ;
        m3mesgc( mesg ) ;
        sprintf( mesg, "fstate[%d]->voffset[ %d ] = %d", f, v, (int)fstate[ f ]->voffset[ v ] ) ;
        m3mesgc( mesg ) ;
        }
    sprintf( mesg, "fstate[%d]->trecsize = %d", f, (int)fstate[ f ]->trecsize ) ;
    m3mesgc( mesg ) ;
    sprintf( mesg, "fstate[%d]->hdrsize  = %d", f, (int)fstate[ f ]->hdrsize ) ;
    m3mesgc( mesg ) ;
    
#endif    

    return( result ) ;

    }           /**  end static void set_fstate()  **/

static void put_state( int f, char * caller )
    {
    int  v ;
    char mesg[ 256 ] ;
    char aname[ NAMLEN3+2 ] ;

    m3mesgc( " " ) ;

    if ( ! fstate[ f ] )
        {
        sprintf( mesg, "BINFIL3 put_state:  file id %d not currently open", f ) ;
        m3mesgc( mesg ) ;
        return ;
        }

    name2cstr( CSTATE3.flist[ f ], aname,
               (FSTR_L)NAMLEN3,    (FSTR_L)NAMLEN3+1 ) ;

    sprintf( mesg, "BINFIL3 putstate() called from %s: file ID %d name %s",
             caller, f, aname ) ;
    m3mesgc( mesg ) ;

    sprintf( mesg, "put_state: hdrsize =  %d", (int) fstate[ f ]->hdrsize ) ;
    m3mesgc( mesg ) ;

    sprintf( mesg, "put_state: trecsize = %d", (int) fstate[ f ]->trecsize ) ;
    m3mesgc( mesg ) ;

    sprintf( mesg, "put_state:    ftype = %d", (int) fstate[ f ]->ftype ) ;
    m3mesgc( mesg ) ;

    sprintf( mesg, "put_state:    fmode = %d", (int) fstate[ f ]->fmode ) ;
    m3mesgc( mesg ) ;

    sprintf( mesg, "put_state:    nrows = %d", (int) fstate[ f ]->nrows ) ;
    m3mesgc( mesg ) ;

    sprintf( mesg, "put_state:    nlays = %d", (int) fstate[ f ]->nlays ) ;
    m3mesgc( mesg ) ;

    sprintf( mesg, "put_state:    nthik = %d", (int) fstate[ f ]->nthik ) ;
    m3mesgc( mesg ) ;

    sprintf( mesg, "put_state:    nvars = %d", (int) fstate[ f ]->nvars ) ;
    m3mesgc( mesg ) ;

    sprintf( mesg, "put_state:    nrecs = %d", (int) fstate[ f ]->nrecs ) ;
    m3mesgc( mesg ) ;

    for ( v = 0 ; v < fstate[ f ]->nvars ; v++ )
        {
        name2cstr( CSTATE3.vlist[ f ][ v ], aname,
               (FSTR_L)NAMLEN3,             (FSTR_L)NAMLEN3+1 ) ;
        sprintf( mesg,
                 "variable %d name %s vtype = %d, vrecsiz = %d, voffset = %d", 
                 v, aname, 
                 (int) fstate[ f ]->vtype[ v ],
                 (int) fstate[ f ]->vrecsiz[ v ],
                 (int) fstate[ f ]->voffset[ v ] ) ;
        m3mesgc( mesg ) ;
        }

    return ;
    }


/********************  BINFIL3-API Functions  *****************************/

/** -------------------- INITBIN3() ----------------------------- **/

FINT INITBIN3( const char *version,
               const FINT *jdate,
               const FINT *jtime,
               FSTR_L      length )
    {
    size_t     n ;
    union { int  ival; INT4  cval ; } iscr ;

    run_date = *jdate ;
    run_time = *jtime ;

    n = (size_t) ( length < NAMLEN3 - 1 ? length : NAMLEN3 - 1 ) ;
    memcpy( ioapi_version, version, n ) ;
    for ( ; n < NAMLEN3 ; n++ )
        {
        ioapi_version[ n ] = ' ' ;
        }

    iscr.cval[0] = (char)0 ;
    iscr.cval[1] = (char)1 ;
    iscr.cval[2] = (char)2 ;
    iscr.cval[3] = (char)3 ;
    byte_order = iscr.ival ;

    for ( n = 0 ; n < MXFILE3+1 ; n++ )
        {
        fstate[ n ] = (Bin_State3 * ) 0 ;
        }

#ifdef BIN3_DEBUG
    m3mesgc( "INITBIN3" ) ;
#endif

    return( (FINT) 1 ) ;

    }   /*  end "int INITBIN3()"  */


/** -------------------- CRTBIN3() ----------------------------- **/

FINT CRTBIN3( const char * fname ,
              const FINT * fid,
              const char * pname,
              FSTR_L       llen, 
              FSTR_L       plen )
    {
    int         i, f, v ;
    int         ncol, nrow, nlay, nthk, nvar, nrec, ftyp, gtyp, mode ;
    int         isiz, fsiz, dsiz ;
    int         vtyp[ MXVARS3 ] ;
    size_t      asize, nsize, nelts ;
    char        fpath[ 512 ] ;
    FILE      * filptr ;
    Bin_Hdr3    bin_hdr ;

    f = *fid - 1 ;

    fstate[ f ] = (Bin_State3*) malloc( (size_t) sizeof( Bin_State3 ) ) ;
    if ( ! fstate[ f ] )
        {
        m3mesgc( "CRTBIN3:  malloc( BIN_STATE ) failure" ) ;
        return( (FINT) 0 ) ;
        }

    ncol = BDESC3.ncols ;
    nrow = BDESC3.nrows ;
    nlay = BDESC3.nlays ;
    nthk = BDESC3.nthik ;
    nvar = BDESC3.nvars ;
    nrec = 0 ;
    ftyp = BDESC3.ftype ;
    gtyp = BDESC3.gdtyp ;
    mode = HDR_MODE ;
    isiz = sizeof( FINT ) ;
    fsiz = sizeof( FREAL ) ;
    dsiz = sizeof( double ) ;

    for( v = 0 ; v < nvar ; v++ )
        {
        vtyp[ v ] = BDESC3.vtype[ v ] ;
        }

    i = set_fstate( f, ncol, nrow, nlay, nthk, nvar, nrec,
                    ftyp, gtyp, vtyp, mode, isiz, fsiz, dsiz ) ;
    if ( i == 0 )
         {
         m3mesgc( "CRTBIN3:  invalid file specification" ) ;
         free( fstate[ f ] ) ;
         fstate[ f ] = (Bin_State3*) 0 ;
         return( (FINT) 0 ) ;
         }

    memcpy(   bin_hdr.ioapi_vrsn,  ioapi_version, (size_t)NAMLEN3 ) ;
    set_int4( bin_hdr.byte_order, (int) byte_order ) ;
    set_int4( bin_hdr.intsize,    (int) sizeof( FINT ) ) ;
    set_int4( bin_hdr.rsize,      (int) sizeof( FREAL ) ) ;
    set_int4( bin_hdr.dsize,      (int) sizeof( double ) ) ;
    memcpy(   bin_hdr.gridname,   CDESC3.gdnam, (size_t)NAMLEN3 ) ;
    memcpy(   bin_hdr.updtname,   CDESC3.upnam, (size_t)NAMLEN3 ) ;
    memcpy(   bin_hdr.file_desc,  CDESC3.fdesc, (size_t)MXDLEN3*MXDESC3 ) ;
    memcpy(   bin_hdr.updt_desc,  CDESC3.updsc, (size_t)MXDLEN3*MXDESC3 ) ;
    set_int4( bin_hdr.cdate,      run_date ) ;
    set_int4( bin_hdr.ctime,      run_time ) ;
    set_int4( bin_hdr.wdate,      run_date ) ;
    set_int4( bin_hdr.wtime,      run_time ) ;
    set_int4( bin_hdr.ftype,      ftyp ) ;
    set_int4( bin_hdr.gdtyp,      gtyp ) ;
    set_int4( bin_hdr.vgtyp,      BDESC3.vgtyp ) ;
    set_int4( bin_hdr.ncols,      ncol ) ;
    set_int4( bin_hdr.nrows,      nrow ) ;
    set_int4( bin_hdr.nlays,      nlay ) ;
    set_int4( bin_hdr.nthik,      nthk ) ;
    set_int4( bin_hdr.nvars,      nvar ) ;
    set_int4( bin_hdr.sdate,      BDESC3.sdate ) ;
    set_int4( bin_hdr.stime,      BDESC3.stime ) ;
    set_int4( bin_hdr.tstep,      BDESC3.tstep ) ;
    set_int4( bin_hdr.nrecs,      (int)0 ) ;
    sprintf(  bin_hdr.p_alpha,    "%27.19e", BDESC3.p_alp ) ;
    sprintf(  bin_hdr.p_beta,     "%27.19e", BDESC3.p_bet ) ;
    sprintf(  bin_hdr.p_gamma,    "%27.19e", BDESC3.p_gam ) ;
    sprintf(  bin_hdr.x_center,   "%27.19e", BDESC3.xcent ) ;
    sprintf(  bin_hdr.y_center,   "%27.19e", BDESC3.ycent ) ;
    sprintf(  bin_hdr.x_origin,   "%27.19e", BDESC3.xorig ) ;
    sprintf(  bin_hdr.y_origin,   "%27.19e", BDESC3.yorig ) ;
    sprintf(  bin_hdr.x_cellsize, "%27.19e", BDESC3.xcell ) ;
    sprintf(  bin_hdr.y_cellsize, "%27.19e", BDESC3.ycell ) ;
    sprintf(  bin_hdr.vgtop,      "%15.9e",  BDESC3.vgtop ) ;

    for( i = 0 ; i <= BDESC3.nlays ; i++ )
        {
        sprintf( bin_hdr.vglvl[ i ], "%15.9e", BDESC3.vglvs[ i ] ) ;
        }

    for( v = 0 ; v < nvar ; v++ )
        {
        set_int4( bin_hdr.vtype[v], vtyp[ v ] ) ;
        }

    name2cstr( fname, fpath, llen, (FSTR_L)512 ) ;
    filptr = fopen( fpath, "w+" ) ;
    if ( ! filptr )
        {
        m3mesgc( "CRTBIN3:  fopen() failure" ) ;
        perror( (char *) 0 ) ;
        free( fstate[ f ] ) ;
        fstate[ f ] = (Bin_State3*) 0 ;
        return( (FINT) 0 ) ;
        }
    fstate[ f ]->fptr = filptr ;

    nsize = sizeof( Bin_Hdr3 ) ;
    nelts = (size_t) 1 ;
    asize = fwrite( & bin_hdr, nsize, nelts, filptr ) ;
    if ( asize != nelts )
        {
        m3mesgc( "CRTBIN3:  fwrite( HEADER ) failure" ) ;
        if ( fclose( filptr ) ) 
            {
            m3mesgc( "CRTBIN3:  fclose() failure" ) ;
            }
        free( filptr ) ;
        free( fstate[ f ] ) ;
        fstate[ f ] = (Bin_State3*) 0 ;
        return( (FINT) 0 ) ;
        }

    nsize = (size_t) NAMLEN3 ;
    nelts = (size_t) nvar ;
    asize = fwrite( (void *)CDESC3.vname, nsize, nelts, filptr ) ;
    if ( asize != nelts )
        {
        m3mesgc( "CRTBIN3:  fwrite( HEADER VNAME ) failure" ) ;
        perror( (char *) 0 ) ;
        if ( fclose( filptr ) ) 
            {
            m3mesgc( "CRTBIN3:  fclose() failure" ) ;
            perror( (char *) 0 ) ;
            }
        free( filptr ) ;
        free( fstate[ f ] ) ;
        fstate[ f ] = (Bin_State3*) 0 ;
        return( (FINT) 0 ) ;
        }

    asize = fwrite( (void *)CDESC3.units, nsize, nelts, filptr ) ;
    if ( asize != nelts )
        {
        m3mesgc( "CRTBIN3:  fwrite( HEADER UNITS ) failure" ) ;
        perror( (char *) 0 ) ;
        
        if ( fclose( filptr ) ) 
            {
            m3mesgc( "CRTBIN3:  fclose() failure" ) ;
            perror( (char *) 0 ) ;
            }
        free( filptr ) ;
        free( fstate[ f ] ) ;
        fstate[ f ] = (Bin_State3*) 0 ;
        return( (FINT) 0 ) ;
        }

    nsize = (size_t) MXDLEN3 ; 
    asize = fwrite( (void *)CDESC3.vdesc, nsize, nelts, filptr ) ;
    if ( asize != nelts )
        {
        m3mesgc( "CRTBIN3:  fwrite( HEADER VDESC ) failure" ) ;
        perror( (char *) 0 ) ;
        if ( 0 != fclose( filptr ) ) 
            {
            m3mesgc( "CRTBIN3:  fclose() failure" ) ;
            perror( (char *) 0 ) ;
            }
        free( filptr ) ;
        free( fstate[ f ] ) ;
        fstate[ f ] = (Bin_State3*) 0 ;
        return( (FINT) 0 ) ;
        }

    if ( BSTATE3.volat[ f ] ) 
        {
        if ( 0 != fsync( fileno( filptr ) ) )
            {
            m3mesgc( "CRTBIN3:  fsync( HEADER VDESC ) failure" ) ;
            perror( (char *) 0 ) ;
            if ( 0 != fclose( filptr ) ) 
                {
                m3mesgc( "CRTBIN3:  fclose() failure" ) ;
                perror( (char *) 0 ) ;
                }
            free( filptr ) ;
            free( fstate[ f ] ) ;
            fstate[ f ] = (Bin_State3*) 0 ;
            return( (FINT) 0 ) ;
            }
        }

    BSTATE3.p_alp[ f ] = BDESC3.p_alp ;
    BSTATE3.p_bet[ f ] = BDESC3.p_bet ;
    BSTATE3.p_gam[ f ] = BDESC3.p_gam ;
    BSTATE3.xcent[ f ] = BDESC3.xcent ;
    BSTATE3.ycent[ f ] = BDESC3.ycent ;
    BSTATE3.xorig[ f ] = BDESC3.xorig ;
    BSTATE3.yorig[ f ] = BDESC3.yorig ;
    BSTATE3.xcell[ f ] = BDESC3.xcell ;
    BSTATE3.ycell[ f ] = BDESC3.ycell ;
    BSTATE3.sdate[ f ] = BDESC3.sdate ;
    BSTATE3.stime[ f ] = BDESC3.stime ;
    BSTATE3.tstep[ f ] = BDESC3.tstep ;

    asize = (size_t)( NAMLEN3 * nvar ) ;
    memcpy( CSTATE3.vlist[ f ], CDESC3.vname, asize ) ;

#ifdef BIN3_DEBUG
    put_state( f, "CRTBIN3" ) ;
#endif

    return( (FINT) 1 ) ;

    }   /*  end "int CRTBIN3()"  */


/** -------------------- OPNBIN3() ----------------------------- **/

FINT OPNBIN3( const char * fname ,
              const FINT * fid,
              const FINT * fmode,
              const char * pname,
              FSTR_L       llen, 
              FSTR_L       plen )
    {
    int         ncol, nrow, nlay, nthk, nvar, nrec, ftyp, gtyp, mode ;
    int         isiz, fsiz, dsiz ;
    int         vtyp[ MXVARS3 ] ;
    int         f, v, i ;
    size_t      asize, nsize, nelts ;
    char        name[ NAMLEN3+1 ] ;
    char        mesg[ 256 ] ;
    char        fpath[ 512 ] ;
    FILE      * filptr ;
    Bin_Hdr3    bin_hdr ;
    unsigned char order[ 5 ] ;

    f = *fid - 1 ;
    fstate[ f ] = (Bin_State3 *) malloc( (size_t) sizeof( Bin_State3 ) ) ;
    if ( ! fstate[ f ] )
        {
        m3mesgc( "OPNBIN3:  malloc( BIN_STATE ) failure" ) ;
        return( (FINT) 0 ) ;
        }

    name2cstr( fname, fpath, llen, (FSTR_L)512 ) ;
    mode = ( *fmode ) % 2 ;     /*  "or" with NF_NOWRITE = 0; NF_WRITE = 1  */
    filptr = fopen( fpath, fmodes[ mode ] ) ;
    if ( ! filptr )
        {
        m3mesgc( "OPNBIN3:  fopen() failure" ) ;
        perror( (char *) 0 ) ;
        free( fstate[ f ] ) ;
        fstate[ f ] = (Bin_State3*) 0 ;
        return( (FINT) 0 ) ;
        }
    fstate[ f ]->fptr = filptr ;

    nsize = sizeof( Bin_Hdr3 ) ;
    nelts = (size_t) 1 ;
    asize = fread( & bin_hdr, nsize, nelts, filptr ) ;
    if ( asize != 1 )
        {
        m3mesgc( "OPNBIN3:  fread( HEADER ) failure" ) ;
        perror( (char *) 0 ) ;
        if ( fclose( filptr ) ) 
            {
            m3mesgc( "CRTBIN3:  fclose() failure" ) ;
            perror( (char *) 0 ) ;
            }
        free( fstate[ f ] ) ;
        free( filptr ) ;
        fstate[ f ] = (Bin_State3*) 0 ;
        return( (FINT) 0 ) ;
        }

    mode = get_int4( bin_hdr.byte_order ) ;
    if ( byte_order !=mode )
        {
        name2cstr( CSTATE3.flist[ f ], name,
                   (FSTR_L)( NAMLEN3 ), (FSTR_L)( NAMLEN3 + 1 ) ) ; 
        m3warnc( "OPNBIN3", 0, 0, "Byte-order mismatch" ) ;
        order[ 4 ] = (char) 0 ;
        set_int4( order, byte_order ) ;
        sprintf( mesg, "Native byte order %s", order ) ;
        set_int4( order, mode ) ;
        sprintf( mesg, "Input  byte order %s for file %s", order, name ) ;
        }

    ncol = get_int4( bin_hdr.ncols ) ;
    nrow = get_int4( bin_hdr.nrows ) ;
    nlay = get_int4( bin_hdr.nlays ) ;
    nthk = get_int4( bin_hdr.nthik ) ;
    nvar = get_int4( bin_hdr.nvars ) ;
    nrec = get_int4( bin_hdr.nrecs ) ;
    ftyp = get_int4( bin_hdr.ftype ) ;
    gtyp = get_int4( bin_hdr.gdtyp ) ;
    mode = (int) RW_MODE ;
    isiz = get_int4( bin_hdr.intsize ) ;
    fsiz = get_int4( bin_hdr.rsize ) ;
    dsiz = get_int4( bin_hdr.dsize ) ;

    if ( nvar > MXVARS3 )
        {
        m3mesgc( "CRTBIN3:  max NVARS exceeded for this build" ) ;
        perror( (char *) 0 ) ;
        if ( fclose( filptr ) ) 
            {
            m3mesgc( "CRTBIN3:  fclose() failure" ) ;
            perror( (char *) 0 ) ;
            }
        free( filptr ) ;
        free( fstate[ f ] ) ;
        fstate[ f ] = (Bin_State3*) 0 ;
        return( (FINT) 0 ) ;
        }

    for( v = 0 ; v < nvar ; v++ )
        {
        vtyp[ v ] = get_int4( bin_hdr.vtype[ v ] ) ;
        }

    i = set_fstate( f, ncol, nrow, nlay, nthk, nvar, nrec,
                    ftyp, gtyp, vtyp, mode, isiz, fsiz, dsiz ) ;

    if ( i == 0 )
         {
         m3mesgc( "OPNBIN3:  invalid file specification" ) ;
         free( fstate[ f ] ) ;
         fstate[ f ] = (Bin_State3*) 0 ;
         return( (FINT) 0 ) ;
         }

    BSTATE3.p_alp[ f ] = strtod( bin_hdr.p_alpha    , (char **)0 ) ;
    BSTATE3.p_bet[ f ] = strtod( bin_hdr.p_beta     , (char **)0 ) ;
    BSTATE3.p_gam[ f ] = strtod( bin_hdr.p_gamma    , (char **)0 ) ;
    BSTATE3.xcent[ f ] = strtod( bin_hdr.x_center   , (char **)0 ) ;
    BSTATE3.ycent[ f ] = strtod( bin_hdr.y_center   , (char **)0 ) ;
    BSTATE3.xorig[ f ] = strtod( bin_hdr.x_origin   , (char **)0 ) ;
    BSTATE3.yorig[ f ] = strtod( bin_hdr.y_origin   , (char **)0 ) ;
    BSTATE3.xcell[ f ] = strtod( bin_hdr.x_cellsize , (char **)0 ) ;
    BSTATE3.ycell[ f ] = strtod( bin_hdr.y_cellsize , (char **)0 ) ;
    BSTATE3.sdate[ f ] = get_int4( bin_hdr.sdate ) ;
    BSTATE3.stime[ f ] = get_int4( bin_hdr.stime ) ;
    BSTATE3.tstep[ f ] = get_int4( bin_hdr.tstep ) ;

    memcpy( CSTATE3.gdnam[ f ],  bin_hdr.gridname, (size_t)NAMLEN3 ) ;

    nsize = (size_t) NAMLEN3 ;
    nelts = (size_t)nvar ;
    asize = fread( (void *)CSTATE3.vlist[ f ], nsize, nelts, filptr ) ;
    if ( asize != nelts )
        {
        m3mesgc( "CRTBIN3:  fread( HEADER VNAME ) failure" ) ;
        perror( (char *) 0 ) ;
        if ( fclose( filptr ) ) 
            {
            m3mesgc( "CRTBIN3:  fclose() failure" ) ;
            perror( (char *) 0 ) ;
            }
        free( filptr ) ;
        free( fstate[ f ] ) ;
        fstate[ f ] = (Bin_State3*) 0 ;
        return( (FINT) 0 ) ;
        }

#ifdef BIN3_DEBUG
    put_state( f, "OPNBIN3" ) ;
#endif

    return( (FINT) 1 ) ;

    }   /*  end "int OPNBIN3()"  */

/** -------------------- DSCBIN3() ----------------------------- **/

FINT DSCBIN3( const FINT * fid )
    {
    int         f, v, nrec ;
    size_t      asize, nsize, nelts ;
    off_t       where ;
    FILE      * filptr ;
    Bin_Hdr3    bin_hdr ;
#ifdef BIN3_DEBUG
    char        mesg[ 256 ] ;
#endif


    f = *fid - 1 ;

    if ( ! fstate[ f ] )
        {
        m3mesgc( "DSCBIN3:  file not yet open" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    filptr = fstate[ f ]->fptr ;
    if ( ! filptr )
        {
        m3mesgc( "DSCBIN3:  bad FID" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    where = (off_t) 0 ;
    if ( 0 != fseeko( filptr, where, SEEK_SET ) )
        {
        m3mesgc( "DSCBIN3:  fseeko( HEADER ) failure" ) ;
        return( (FINT) 0 ) ;
        }

    nsize = sizeof( Bin_Hdr3 ) ;
    nelts = (size_t) 1 ;
    asize = fread( & bin_hdr, nsize, nelts, filptr ) ;
    if ( asize != 1 )
        {
        m3mesgc( "DSCBIN3:  fread( HEADER ) failure" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    nrec = get_int4( bin_hdr.nrecs ) ;
    BSTATE3.mxrec[ f ] = nrec ;

    memcpy( CDESC3.upnam, bin_hdr.updtname,  (size_t)  NAMLEN3 ) ;
    memcpy( CDESC3.execn, bin_hdr.execution, (size_t)  MXDLEN3 ) ;
    memcpy( CDESC3.fdesc, bin_hdr.file_desc, (size_t)( MXDLEN3 * MXDESC3 ) ) ;
    memcpy( CDESC3.updsc, bin_hdr.updt_desc, (size_t)( MXDLEN3 * MXDESC3 ) ) ;

    BDESC3.cdate = get_int4( bin_hdr.cdate ) ;
    BDESC3.ctime = get_int4( bin_hdr.ctime ) ;
    BDESC3.wdate = get_int4( bin_hdr.wdate ) ;
    BDESC3.wtime = get_int4( bin_hdr.wtime ) ;
    BDESC3.vgtyp = get_int4( bin_hdr.vgtyp ) ;

    sscanf(  bin_hdr.vgtop, "%e",  & BDESC3.vgtop ) ;
    for( v = 0 ; v < fstate[ f ]->nlays + 1 ; v++ )
        {
        sscanf( bin_hdr.vglvl[ v ], "%e", & BDESC3.vglvs[ v ] ) ;
        }

    nsize = (size_t) NAMLEN3 ;
    nelts = (size_t) fstate[ f ]->nvars ;
    asize = fread( (void *)CDESC3.vname, nsize, nelts, filptr ) ;
    if ( asize != nelts )
        {
        m3mesgc( "DSCBIN3:  fread( HEADER VNAME ) failure" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
    }

    asize = fread( (void *)CDESC3.units, nsize, nelts, filptr ) ;
    if ( asize != nelts )
        {
        m3mesgc( "DSCBIN3:  fread( HEADER UNITS ) failure" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
    }

    nsize = (size_t) MXDLEN3 ; 
    asize = fread( (void *)CDESC3.vdesc, nsize, nelts, filptr ) ;
    if ( asize != nelts )
        {
        m3mesgc( "DSCBIN3:  fread( HEADER VDESC ) failure" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
    }

#ifdef BIN3_DEBUG
    sprintf( mesg,
             "DSCBIN3:  file  %d", f ) ;
    m3mesgc( mesg ) ;
#endif

    return( (FINT) 1 ) ;

    }   /*  end "int DSCBIN3()"  */

/** -------------------- RDBFLAG() ----------------------------- **/

FINT RDBFLAG( const FINT * fid,
              const FINT * vid,
              const FINT * step,
              FINT         flags[] )
    {
    int         f, v, s, n ;
    off_t       where ;
    size_t      asize, rsize, nsize ;
    INT4        tflag[ 2 * MXVARS3 ] ;
    FILE      * filptr ;
#ifdef BIN3_DEBUG
    char        mesg[ 256 ] ;
#endif

    f = *fid  - 1 ;
    v = *vid  - 1 ;
    s = *step - 1 ;
    if ( ! fstate[ f ] )
        {
        m3mesgc( "RDBFLAG:  file not yet open" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    filptr = fstate[ f ]->fptr ;
    if ( ! filptr )
        {
        m3mesgc( "RDBFLAG:  bad FID" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    fstate[ f ]->fmode = RW_MODE ;
    where  = fstate[ f ]->hdrsize + s * fstate[ f ]->trecsize ;
    if ( v < 0 )                /**  "read all variables" case  **/
        {
        n     = ( fstate[ f ]->nvars > 0 ? fstate[ f ]->nvars : 1 ) ;
        rsize = (size_t)( 2 * n ) ;
        }
    else{                       /**  "read one variable" case  **/
        rsize = (size_t)( 2 ) ;
        where += (off_t)( 2 * v * sizeof( INT4 ) ) ;
        }

    if ( 0 != fseeko( filptr, where, SEEK_SET ) )
        {
        m3mesgc( "RDBFLAG:  fseeko( TIMESTEP HEADER ) failure" ) ;
        return( (FINT) 0 ) ;
        }

    nsize = sizeof( INT4 ) ;
    asize = fread( (void *) tflag[ 0 ], nsize, rsize, filptr ) ;
    if ( asize != rsize )
        {
        m3mesgc( "RDBFLAG:  fread( TIMESTEP HEADER ) failure" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    for ( n = 0 ; n < asize ; n++ )
        {
        flags[ n ] = (FINT) get_int4( tflag[ n ] ) ;
        }

#ifdef BIN3_DEBUG
    sprintf( mesg,
             "RDBFLAG:  file  %d  where = %d  nsize = %d rsize = %d",
             f, (int) where, (int) nsize, (int) rsize ) ;
    m3mesgc( mesg ) ;
#endif

    return( (FINT) 1 ) ;

    }   /*  end "int RDBFLAG()"  */

/** -------------------- WRBFLAG() ----------------------------- **/

FINT WRBFLAG( const FINT * fid,
              const FINT * vid,
              const FINT * step,
              FINT         flags[] )
    {
    int         f, v, nrec, n, s, t ;
    off_t       where ;
    size_t      asize, rsize, nsize ;
    INT4        tflag[ 2 * MXVARS3 ] ;
    INT4        mxrec ;
    FILE      * filptr ;
#ifdef BIN3_DEBUG
    char        mesg[ 256 ] ;
#endif

    f = *fid  - 1 ;
    v = *vid  - 1 ;
    s = *step - 1 ;
    if ( ! fstate[ f ] )
        {
        m3mesgc( "WRBFLAG:  file not yet open" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    filptr = fstate[ f ]->fptr ;
    if ( ! filptr )
        {
        m3mesgc( "WRBFLAG:  bad FID" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    fstate[ f ]->fmode = RW_MODE ;
    where  = fstate[ f ]->hdrsize + s * fstate[ f ]->trecsize ;
    if ( v < 0 )
        {
        n     = ( fstate[ f ]->nvars > 0 ? fstate[ f ]->nvars : 1 ) ;
        rsize = (size_t)( 2 * n ) ;
        }
    else{
        rsize = (size_t)( 2 ) ;
        where += (off_t)( 2 * v * sizeof( INT4 ) ) ;
        }

    if ( 0 != fseeko( filptr, where, SEEK_SET ) )
        {
        m3mesgc( "RDBFLAG:  fseeko( TIMESTEP HEADER ) failure" ) ;
        return( (FINT) 0 ) ;
        }

    for ( n = 0 ; n < rsize ; n++ )
        {
        t = (int)( flags[ n % 2 ] ) ;
        set_int4( tflag[ n ] , t ) ;
        }

    nsize = sizeof( INT4 ) ;
    asize = fwrite( (void *) tflag[ 0 ] , nsize, rsize, filptr ) ;
    if ( asize != rsize )
        {
        m3mesgc( "WRBFLAG:  fwrite( TIMESTEP HEADER ) failure" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

#ifdef BIN3_DEBUG
    sprintf( mesg,
             "WRBFLAG:  file  %d  where = %d  nsize = %d rsize = %d",
             f, (int)  where, (int) nsize, (int) rsize ) ;
    m3mesgc( mesg ) ;
#endif

    nrec = fstate[f]->nrecs ;
    if ( nrec < s + 1 )
        {
        nrec = s + 1 ;
        set_int4( mxrec, (int) nrec ) ;

        where = (off_t) 0 ;
            if ( 0 != fseeko( filptr, where, SEEK_SET ) )
            {
            m3mesgc( "WRBFLAG:  fseeko( MXREC ) failure" ) ;
            return( (FINT) 0 ) ;
            }

        rsize = (size_t) 1 ;
        asize = fwrite( (void *) mxrec, nsize, rsize, filptr ) ;
        if ( asize != rsize )
            {
            m3mesgc( "RDBFLAG:  fwrite( MXREC ) failure" ) ;
            perror( (char *) 0 ) ;
            return( (FINT) 0 ) ;
            }
        fstate[f]->nrecs   = nrec ;
        BSTATE3.mxrec[ f ] = nrec ;
        }
    
    return( (FINT) 1 ) ;

    }   /*  end "int WRBFLAG()"  */

/** -------------------- RDBVARS() ----------------------------- **/

FINT RDBVARS( const FINT * fid,
              const FINT * var,
              const FINT * lay,
              const FINT * step,
              void       * buffer )
    {
    int         f, v, w, l, s, ftyp ;
    off_t       now, where ;
    size_t      asize, nsize, rsize, tsize ;
    FILE      * filptr ;
    char      * bptr ;
#ifdef BIN3_DEBUG
    char        mesg[ 256 ] ;
#endif

    f = *fid  - 1 ;
    v = *var  - 1 ;
    l = *lay  - 1 ;
    s = *step - 1 ;

#ifdef  BIN3_DEBUG
    sprintf( mesg, "RDBVARS:  FILE %d LAYER = %d VBLE = %d STEP = %d", 
                              f,      l,         v,        s ) ;
#endif

    if ( ! fstate[ f ] )
        {
        m3mesgc( "RDBVARS:  file not yet open" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    filptr = fstate[ f ]->fptr ;
    if ( ! filptr )
        {
        m3mesgc( "RDBVARS:  bad FID" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    fstate[ f ]->fmode = RW_MODE ;
    where  = fstate[ f ]->hdrsize + s*fstate[ f ]->trecsize ;

                        /**  First, the entire-timestep-record cases:  **/

    ftyp = fstate[ f ]->ftype ;

    if ( ( ftyp == IDDATA3 ) || ( ftyp == PROFIL3 ) ||
         ( ftyp == GRNEST3 ) || ( ftyp == SMATRX3 ) ||
         ( v < 0 && l < 0 ) )
        {
        tsize = fstate[ f ]->voffset[ 0 ] ;
        where += tsize ;
        rsize = fstate[ f ]->trecsize - tsize ;
        }

    else if ( v >= 0 )  /* next, the single-variable read operations  */
        {
        if ( l < 0 )    /* single-variable/all-layers read operation  */
            {
            rsize  = fstate[ f ]->vrecsiz[ v ] * fstate[ f ]->nlays ;
            where += fstate[ f ]->voffset[ v ] ;
            }
        else            /* single-variable/single-layer read operation  */
            {
            rsize  = fstate[ f ]->vrecsiz[ v ] ;
            where += fstate[ f ]->voffset[ v ] + l * fstate[ f ]->vrecsiz[ v ] ;
            }
        }
                        /*  all-variable/single-layer read operation  */
    else{               /*  Must be done iteratively:  */

        nsize = sizeof( char ) ;
        for( bptr = buffer, w = 0 ; w < fstate[ f ]->nvars ; w++ )
            {
            rsize = fstate[ f ]->vrecsiz[ w ] ;
            now = where + fstate[ f ]->voffset[ w ] + l * rsize ;
            if ( 0 != fseeko( filptr, now, SEEK_SET ) )
                {
                m3mesgc( "RDBVARS:  fseeko( V-TIMESTEP ) failure" ) ;
                return( (FINT) 0 ) ;
                }

            asize = fread( bptr, nsize, rsize, filptr ) ;
            if ( asize != rsize )
                {
                m3mesgc( "RDBVARS:  fread( V-TIMESTEP ) failure" ) ;
                perror( (char *) 0 ) ;
                return( (FINT) 0 ) ;
                }
            bptr += rsize ;
            }

        return( (FINT) 1 ) ;

        }   /**  end iterative all-variable/single-layer read operation  */

#ifdef  BIN3_DEBUG
    sprintf( mesg, "RDBVARS:  NSIZE = %d ", (int) nsize ) ;
    m3mesgc( mesg ) ;

    sprintf( mesg, "RDBVARS:  RSIZE = %d ", (int) rsize ) ;
    m3mesgc( mesg ) ;

    if ( v < 0 ) 
        {
        sprintf( mesg, "RDBVARS:  OFFSET = %d ", (int) fstate[ f ]->voffset[ 0 ] ) ;
        m3mesgc( mesg ) ;

        sprintf( mesg, "RDBVARS:  RECSIZ = %d ", (int) fstate[ f ]->vrecsiz[ 0 ] ) ;
        m3mesgc( mesg ) ;
        }
    else
        {
        sprintf( mesg, "RDBVARS:  VOFFSET = %d ", (int) fstate[ f ]->voffset[ v ] ) ;
        m3mesgc( mesg ) ;

        sprintf( mesg, "RDBVARS:  VRECSIZ = %d ", (int) fstate[ f ]->vrecsiz[ v ] ) ;
        m3mesgc( mesg ) ;
        }

    sprintf( mesg, "RDBVARS:  WHERE = %d ", (int) where ) ;
    m3mesgc( mesg ) ;

#endif

    if ( 0 != fseeko( filptr, where, SEEK_SET ) )
        {
        m3mesgc( "RDBVARS:  fseeko( TIMESTEP ) failure" ) ;
        return( (FINT) 0 ) ;
        }

    nsize = sizeof( char ) ;
    asize = fread( buffer, nsize, rsize, filptr ) ;
    if ( asize != rsize )
        {
        m3mesgc( "RDBVARS:  fread( TIMESTEP ) failure" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

#ifdef BIN3_DEBUG
    sprintf( mesg,
             "RDBVARS:  file  %d  where = %d  nsize = %d rsize = %d",
             f, (int)  where, (int) nsize, (int) rsize ) ;
    m3mesgc( mesg ) ;
#endif

    return( (FINT) 1 ) ;

    }   /*  end "int RDBVARS()"  */

/** -------------------- WRBVARS() ----------------------------- **/

FINT WRBVARS( const FINT * fid,
              const FINT * vid,
              const FINT * step,
              void       * buffer )
    {
    int         f, v, s, ftyp ;
    off_t       where, tsize ;
    size_t      asize, nsize, rsize ;
    FILE      * filptr ;
#ifdef BIN3_DEBUG
    char        mesg[ 256 ] ;
#endif

    f    = *fid  - 1 ;
    v    = *vid  - 1 ;
    s    = *step - 1 ;

    if ( ! fstate[ f ] )
        {
        m3mesgc( "WRBVARS:  file not yet open" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    filptr = fstate[ f ]->fptr ;
    if ( ! filptr )
        {
        m3mesgc( "WRBVARS:  bad FID" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    fstate[ f ]->fmode = RW_MODE ;
    where  = fstate[ f ]->hdrsize + s*fstate[ f ]->trecsize ;
    
    ftyp = fstate[ f ]->ftype ;

    if ( ( ftyp == IDDATA3 ) || ( ftyp == PROFIL3 ) ||
         ( ftyp == GRNEST3 ) || ( ftyp == SMATRX3 ) ||
         ( v < 0 ) )
        {
        tsize = fstate[ f ]->voffset[ 0 ]  ;
        where += tsize ;
        rsize = fstate[ f ]->trecsize - tsize ;
        }
    else{                       /*  single-variable write operation  */
        rsize  = fstate[ f ]->vrecsiz[ v ] * fstate[ f ]->nlays ;
        where += fstate[ f ]->voffset[ v ] ;
        }

    if ( 0 != fseeko( filptr, where, SEEK_SET ) )
        {
        m3mesgc( "WRBVARS:  fseeko( TIMESTEP ) failure" ) ;
        return( (FINT) 0 ) ;
        }

    nsize = sizeof( char ) ;
    asize = fwrite( buffer, nsize, rsize, filptr ) ;
    if ( asize != rsize )
        {
        m3mesgc( "WRBVARS:  fwrite( TIMESTEP ) failure" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

#ifdef BIN3_DEBUG
    sprintf( mesg,
             "WRBVARS:  file  %d  where = %d  nsize = %d rsize = %d",
             f, (int)  where, (int) nsize, (int) rsize ) ;
    m3mesgc( mesg ) ;
#endif

    return( (FINT) 1 ) ;

    }   /*  end "int WRBVARS()"  */

/** -------------------- XTRBIN3() ----------------------------- **/

FINT XTRBIN3( const FINT * fid,
              const FINT * var,
              const FINT * lay0, const FINT * lay1,
              const FINT * row0, const FINT * row1,
              const FINT * col0, const FINT * col1,
              const FINT * step,
              void       * buffer )
    {
    int         f, n, v, r, l, t ;
    int         l0, r0, c0, l1, r1, c1 ;
    off_t       where ;
    size_t      s, ss, ds, dd, dw, di, asize, rsize, lsize, vsize, nsize ;
    FILE      * filptr ;
    char      * dptr, * sptr, * lsptr ;
    static char   * inbuf = (char *) 0 ;
    static size_t   last_size = 0 ;
#ifdef BIN3_DEBUG
    char        mesg[ 256 ] ;
#endif

    f  = *fid - 1 ;
    v  = *var - 1 ;
    l0 = *lay0 ;
    r0 = *row0 ;
    c0 = *col0 ;
    l1 = *lay1 ;
    r1 = *row1 ;
    c1 = *col1 ;
    
    if ( ! fstate[ f ] )
        {
        m3mesgc( "XTRBIN3:  file not yet open" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    filptr = fstate[ f ]->fptr ;
    if ( ! filptr )
        {
        m3mesgc( "XTRBIN3:  bad FID" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    fstate[ f ]->fmode = RW_MODE ;

    lsize = BSTATE3.ncols[ f ] * BSTATE3.nrows[ f ] ;
    vsize = lsize * ( l1 - l0 + 1 ) ;
    if ( vsize > last_size )
        {
        if ( inbuf ) free( inbuf ) ;
        inbuf = malloc( vsize  * sizeof( double ) ) ;
        if ( inbuf )
            {
            last_size = vsize ;
            }
        else{
            last_size = 0 ;
            m3mesgc( "XTRBIN3:  malloc() failure" ) ;
            perror( (char *) 0 ) ;
            return( (FINT) 0 ) ;
            }
        }

    where = fstate[ f ]->hdrsize + (*step - 1)*fstate[ f ]->trecsize ;
    nsize = sizeof( char ) ;

    if ( v == ALLAYS3 )                 /*   multi-vble extract  */
        {
        for ( n = 0, dptr = buffer ; n < BSTATE3.nvars[ f ] ; n++ )
            {
            t      = BSTATE3.vtype[ f ][ n ] ;
            s      = tsizes[ t ] ;
            dd     = s * ( c1 - c0 + 1 ) ;                              /*  row size           */
            ds     = s * lsize ;                                        /*  layer size         */
            dw     = ds * ( *lay0 - 1 ) + fstate[ f ]->voffset[ n ] ;   /*  layer+vble offset  */
            di     = s * ( BSTATE3.ncols[ f ] * ( r0 - 1 ) + c0 - 1 ) ; /*  (c,r) offset       */
            rsize  = s * lsize * ( l1 - l0 + 1 ) ;                      /*  volume to read     */

            if ( 0 != fseeko( filptr, where + dw, SEEK_SET ) )
                {
                m3mesgc( "XTRBIN3:  fseeko( TIMESTEP ) failure" ) ;
                perror( (char *) 0 ) ;
                free( inbuf ) ;
                return( (FINT) 0 ) ;
                }

            asize = fread( (void *)inbuf, nsize, rsize, filptr ) ;
            if ( asize != rsize )
                {
                m3mesgc( "XTRBIN3:  fread( TIMESTEP ) failure" ) ;
                perror( (char *) 0 ) ;
                free( inbuf ) ;
                return( (FINT) 0 ) ;
                }

#ifdef BIN3_DEBUG
            sprintf( mesg,
                     "XTRBIN3:  file  %d  where = %d  nsize = %d rsize = %d",
                     f, (int)  where, (int) nsize, (int) rsize ) ;
            m3mesgc( mesg ) ;
#endif

            for ( sptr = inbuf + di, l = l0 - 1 ; l < l1 ; l++, sptr+=ds )
                {
                for ( r = r0-1, lsptr = sptr; r < r1; r++, lsptr+=ss, dptr+=dd )
                    {
                    memcpy( (void *)dptr, (void *)lsptr, dd ) ;
                    }
                }                       /*  end loop on layers l  */
            }                           /*  end loop on vbles  n */
        }                               /*  if multi-vble extract  */

    else{                               /*  else single-vble extract  */

        t      = BSTATE3.vtype[ f ][ v ] ;
        s      = tsizes[ t ] ;
        ss     = s * BSTATE3.ncols[ f ] ;       /*   intput row size  */
        dd     = s * ( c1 - c0 + 1 ) ;          /*  output row size   */
        ds     = s * lsize ;                    /*  input layer-size  */
        rsize  = ds * ( *lay1 - *lay0 + 1 ) ;   /*  volume to read    */
        dw     = s * lsize * ( *lay0 - 1 ) ;    /*  layer offset in file */
        where += fstate[ f ]->voffset[ v ] ;    /*  vble  offset in file */
        di     = s * ( BSTATE3.ncols[ f ] * ( r0 - 1 ) + c0 - 1 ) ;

        if ( 0 != fseeko( filptr, where + dw, SEEK_SET ) )
            {
            m3mesgc( "XTRBIN3:  fseeko( TIMESTEP ) failure" ) ;
            perror( (char *) 0 ) ;
            free( inbuf ) ;
            return( (FINT) 0 ) ;
            }

        asize = fread( (void *)inbuf, nsize, rsize, filptr ) ;
        if ( asize != rsize )
            {
            m3mesgc( "XTRBIN3:  fread( TIMESTEP ) failure" ) ;
            perror( (char *) 0 ) ;
            free( inbuf ) ;
            return( (FINT) 0 ) ;
            }

#ifdef BIN3_DEBUG
    sprintf( mesg,
             "XTRBIN3:  file  %d  where = %d  nsize = %d rsize = %d",
             f, (int) where, (int) nsize, (int) rsize ) ;
    m3mesgc( mesg ) ;
#endif

        for ( sptr=inbuf+di, dptr=buffer, l = l0 - 1 ; l < l1 ; l++, sptr+=ds )
            {
            for ( r = r0 - 1, lsptr = sptr ; r < r1 ; r++, lsptr+=ss, dptr+=dd )
                {
                memcpy( (void *)dptr, (void *)lsptr, dd ) ;
                }
            }           /*  end loop on layers l  */

        }               /*  if multi-vble extract, or not  */

    return( (FINT) 1 ) ;

    }   /*  end "int XTRBIN3()"  */

/** -------------------- FLUSHBIN3() ----------------------------- **/

FINT FLUSHBIN3( const FINT * fid )
    {
    FILE *filptr ;
    int   f ;
#ifdef BIN3_DEBUG
    char  mesg[ 256 ] ;
#endif

    f = *fid - 1 ;
    if ( ! fstate[ f ] )
        {
        m3mesgc( "FLUSHBIN3:  file not yet open" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    filptr = fstate[ f ]->fptr ;
    if ( ! filptr )
        {
        m3mesgc( "FLUSHBIN3:  bad FID" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    if ( 0 != fsync( fileno( filptr ) ) )
        {
        m3mesgc( "FLUSHBIN3:  fsync() failure" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

#ifdef BIN3_DEBUG
    sprintf( mesg, "FLUSHBIN3:  file  %d", f ) ;
    m3mesgc( mesg ) ;
#endif

    return( (FINT) 1 ) ;

    }   /*  end "int FLUSHBIN3()"  */

/** -------------------- CLOSEBIN3() ----------------------------- **/

FINT CLOSEBIN3( const FINT * fid )
    {
    FILE *filptr ;
    int   f ;
#ifdef BIN3_DEBUG
    char  mesg[ 256 ] ;
#endif

    f = *fid - 1 ;
    if ( ! fstate[ f ] )
        {
        m3mesgc( "CLOSEBIN3:  file not yet open" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    filptr = fstate[ f ]->fptr ;
    if ( ! filptr )
        {
        m3mesgc( "CLOSEBIN3:  bad FID" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    if ( 0 != fclose( filptr ) ) 
        {
        m3mesgc( "CLOSEBIN3:  fclose() failure" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

#ifdef BIN3_DEBUG
    sprintf( mesg,
             "CLOSEBIN3:  file  %d", f ) ;
    m3mesgc( mesg ) ;
#endif

    free( fstate[ f ] ) ;
    fstate[ f ] = (Bin_State3*) 0 ;

    return( (FINT) 1 ) ;

    }   /*  end "int CLOSEBIN3()"  */

/** -------------------- WRBPATCH() ----------------------------- **/

FINT WRBPATCH( const FINT * fid,
               const FINT * vid,
                     FINT grid_map[],
                     FINT size_map[],
               const FINT * step,
               void       * patch )

    {
    int         f, v, s, l, r, r0, r1, c0, nlay, ncol;
    off_t       where, start, rowsiz;
    size_t      asize, chsize, rsize, prsize, tsize ;
    FILE      * filptr ;
    char      * buffer ;
#ifdef BIN3_DEBUG
    char        mesg[ 256 ] ;
#endif

    f = *fid  - 1 ;
    v      = *vid  - 1 ;
    s      = *step - 1 ;

    if ( ! fstate[ f ] )
        {
        m3mesgc( "WRBPATCH:  file not yet open" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    filptr = fstate[ f ]->fptr ;
    if ( ! filptr )
        {
        m3mesgc( "WRBPATCH:  bad FID" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }

    if ( v < 0 )        /*  multi-variable case:  */
        {
        m3mesgc( "WRBPATCH:  ALLVARS write is not supported" ) ;
        perror( (char *) 0 ) ;
        return( (FINT) 0 ) ;
        }                       /*  end loop on v  */

    fstate[ f ]->fmode = RW_MODE ;      /*  not header-mode  */

    c0     = grid_map[0] - 1 ;                  /*  Starting C subscripts  */
    r0     = grid_map[1] - 1 ;
    r1     = r0 + size_map[1] ;                 /*  Upper subscript bound  */
    nlay   = fstate[ f ]->nlays ;
    ncol   = fstate[ f ]->ncols ;
    tsize  = tsizes[ fstate[ f ]->vtype[ v ] ] ;
    rsize  = fstate[ f ]->vrecsiz[ v ] ;
    start  = fstate[ f ]->hdrsize + s*fstate[ f ]->trecsize
                                  +   fstate[ f ]->voffset[ v ]
                                  + tsize * ( ncol*r0 + c0 );
    rowsiz = tsize * ncol ;             /* row size in file  */
    prsize = tsize * size_map[0]  ;     /* row size in patch */
    chsize = sizeof( char ) ;
    buffer = (char *) patch ;           /*  patch-data pointer  */

    for ( l = 0 ; l < nlay ; l++ )
        { 
        where = start + l*rsize ;       /*  file-data pointer  */
        for ( r = r0 ; r  < r1 ; r++ )
            { 
            if ( 0 != fseeko( filptr, where, SEEK_SET ) )
                {
                m3mesgc( "WRBPATCH:  single-vble fseeko() failure" ) ;
                return( (FINT) 0 ) ;
                }

            asize = fwrite( buffer, chsize, prsize, filptr ) ;

            if ( asize != prsize )
               {
               m3mesgc( "WRBPATCH:  single-vble fwrite() failure" ) ;
               perror( (char *) 0 ) ;
               return( (FINT)   0 ) ;
               }

            buffer += prsize ;      /* advance the buffer locations */
            where  += rowsiz ;
            }               /*  end loop on r  */
        }                   /*  end loop on l  */


#ifdef BIN3_DEBUG
    sprintf( mesg,
             "WRBPATCH:  file  %d  where = %d  csize = %d rsize = %d",
             f, (int)  where, (int) chsize, (int) rsize ) ;
    m3mesgc( mesg ) ;
#endif

    return( (FINT) 1 ) ;

    }   /*  end "int WRBPATCH()"  */


