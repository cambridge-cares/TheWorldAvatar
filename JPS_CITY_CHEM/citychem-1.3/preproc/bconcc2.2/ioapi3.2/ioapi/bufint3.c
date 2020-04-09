/**************************************************************************
VERSION "$Id: bufint3.c 1 2017-06-10 18:05:20Z coats $"
    EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010 Baron Advanced Meteorological Systems, and
    (C) 2015 UNC Institute for the Environment.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

public  function BUFVGT3()  starts at line  165
public  function BUFVGT3D() starts at line  217
public  function BUFVRD3()  starts at line  274
public  function BUFVRD3D() starts at line  332
public  function BUFINT3()  starts at line  393
public  function BUFINT3D() starts at line  491
public  function BUFCRE3()  starts at line  592
public  function BUFPUT3()  starts at line  633
public  function BUFPUT3I() starts at line  679
public  function BUFPUT3D() starts at line  725
public  function BUFGET3()  starts at line  771
public  function BUFGET3I() starts at line  816
public  function BUFGET3D() starts at line  861
public  function BUFXTR3()  starts at line  906
public  function BUFXTR3I() starts at line  964
public  function BUFXTR3D() starts at line 1022
public  function BUFDDT3()  starts at line 1080
public  function BUFDDT3D() starts at line 1122
public  function BUFDEL3()  starts at line 1169
public  function BUFINTX()  starts at line 1189
public  function BUFINTXD() starts at line 1299

NOTE:
    !!!  MACHINE-DEPENDENT CODE !!!  Dependent upon the
    representation of Fortran LOGICAL  variables
   .TRUE. as binary all-ones, and .FALSE. as zero.

PURPOSE:
   Supports INTERP3.FOR and buffering for "virtual files" to be
   used as structured communications channels.
   Should be considered the buffer management part of "INTERP3()".
   Interpolates specified variable from file with specified ID
   according to time step and interpolation coefficients p, q,
   putting the result in the specified buffer.
   Allocates and manages circular buffers used in the interpolation,
   and performs reads (using RDVARS()) whenever specified by rflag.

RETURN VALUE:
   TRUE iff the operation succeeds (and the data is available)

PRECONDITIONS:
   Correct set-up by INTERP3.F.

CALLS: 
    I/O API Fortran-binding routine RDVARS()

CALLED BY:
    CLOSE3(), INTERP3(), READ3(), WRITE3(), XTRACT3()

REVISION HISTORY:
    Prototype 4/1993 by CJC
    Modified  8/1994 by CJC:  BUFFERED "virtual files" with CREATE3(),
                              READ3(), WRITE3(), INTERP3(), XTRACT3()
    Modified 10/1994 by CJC:  write for individual variables
    Modified  8/1995 by CJC:  support for CLOSE3()
    Modified  5/1998 by CJC for OpenMP thread-safety
    Modified  2/2002 by CJC:  integration with per-patch/extracting
    INTERPX() from Jeff Young, US EPA/ORD
    Modified  5/2002 by CJC:  support for variables of type DOUBLE
    Modified  2/2003 by CJC:  bug fix for BUFINTX, BUFINTXD.  Bug
    found in BUFINTX by David Wong, US EPA.
    Modified 10/2003 by CJC for I/O APIv3:  cross-language FINT/FSTR_L
    type resolution modifications, BINFIL3 input
    Modified 9/2004 by CJC:  virtual-file bugfix -- new BUFVRD3, BUFVRD3D,
    add allocation facilites to BUFPUT* routines; new interface to READ3V.
    Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
    upper-case Fortran  symbols, prepend _C to common blocks.
    Modified 04/2011 for full buffered-file file descriptions.
    Modified 02/2015 by CJC for I/O API version 3.2:  M3INT8 (INTEGER*8) support
**************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "parms3.h"
#include "iodecl3.h"

#define VALXTR3( L,R,C )  ( *( bptr + nc*( nr*(L) + (R) ) + (C) ) )
    

/**  DEAL WITH  FELDMANISMS  OF MANY UN*X F77'S   **/

#if FLDMN

#define  RDVARS    rdvars_
#define  READ3V    read3v_

#define  BUFVGT3   bufvgt3_
#define  BUFVGT3D  bufvgt3d_
#define  BUFVRD3   bufvrd3_
#define  BUFVRD3D  bufvrd3d_
#define  BUFCRE3   bufcre3_
#define  BUFDEL3   bufdel3_
#define  BUFDDT3   bufddt3_
#define  BUFDDT3D  bufddt3d_
#define  BUFGET3   bufget3_
#define  BUFGET3D  bufget3d_
#define  BUFGET3I  bufget3i_
#define  BUFINT3   bufint3_
#define  BUFINT3D  bufint3d_
#define  BUFPUT3   bufput3_
#define  BUFPUT3D  bufput3d_
#define  BUFPUT3I  bufput3i_
#define  BUFXTR3   bufxtr3_
#define  BUFXTR3D  bufxtr3d_
#define  BUFXTR3I  bufxtr3i_
#define  BUFINTX   bufintx_
#define  BUFINTXD  bufintxd_

#elif defined(__hpux) || defined(_AIX)

#define  RDVARS    rdvars
#define  READ3V    read3v

#define  BUFVGT3   bufvgt3
#define  BUFVGT3D  bufvgt3d
#define  BUFVRD3   bufvrd3
#define  BUFVRD3D  bufvrd3d
#define  BUFCRE3   bufcre3
#define  BUFDEL3   bufdel3
#define  BUFDDT3   bufddt3
#define  BUFDDT3D  bufddt3d
#define  BUFGET3   bufget3
#define  BUFGET3D  bufget3d
#define  BUFGET3I  bufget3i
#define  BUFINT3   bufint3
#define  BUFINT3D  bufint3d
#define  BUFPUT3   bufput3
#define  BUFPUT3D  bufput3d
#define  BUFPUT3I  bufput3i
#define  BUFXTR3   bufxtr3
#define  BUFXTR3D  bufxtr3d
#define  BUFXTR3I  bufxtr3i
#define  BUFINTX   bufintx
#define  BUFINTXD  bufintxd

#elif defined(ABSFT)

        /*  do nothing!  */
        /*  Absoft uses ALL_CAP name mangling, Feldman-style strings */

#else

#error   "Error compiling envgets.c:  unsupported architecture"

#endif


/** EXTERNAL FORTRAN FUNCTIONS FOR READING NETCDF, PVM DATA: **/

        extern FINT RDVARS( FINT *fid,    FINT *vid,
                            FINT  dims[], FINT delts[], FINT *delta,
                            void *buffer ) ;

#ifdef IOAPICPL

        extern FINT READ3V( FINT *fid,   FINT *vid,   FINT *skip,
                            FINT *kdate, FINT *ktime, void *buffer,
                            FINT *count ) ;

#endif          /**  ifdef IOAPICPL  **/


/*****************  STATE VARIABLES ************************************/

    /** BUFFER ADDRS, OR 0:  NOTE THAT FORTRAN (1-BASED) SUBSCRIPTING USED **/

    static void *baddr[ MXFILE3+1 ][ MXVARS3+1 ] ;


/*****************  BUFVGT3: *****************************************/
/** Read physical-file data for INTERP3() and DDTVAR3() (type FREAL) **/
    
FINT BUFVGT3 ( FINT  *fndx,      /** M3 file index **/
               FINT  *vndx,      /** M3 variable index **/
               FINT  *rflag,     /** read-only flag **/
               FINT   dims[],    /** hyperslab corner   arg for NCVGT() **/
               FINT   delt[],    /** hyperslab diagonal arg for NCVGT() **/
               FINT  *bsize,     /** buffer size **/
               FINT  *delta,     /** offset for read-op **/
               FINT  *tstep )    /** TRUE iff time-dependent file **/

    {  /**  begin body of bufvgt3() **/

    FINT   size ;               /** SCRATCH VARIABLE:  BUFFER SIZE  **/
    FINT   ierr ;               /** RETURN FLAG FOR NCVGT()  **/
    FREAL *bptr ;               /** FOR TRAVERSING INTERPOLATION BUFFER **/
    char   mesg[ 81 ] ;

    if ( ! ( bptr = (FREAL *)baddr[ *fndx ][ *vndx ] ) )   /** NOT YET ALLOCATED **/
        {
        size = (FINT)( ( *tstep ? 2 : 1 ) * sizeof( FREAL ) * (*bsize ) );
        baddr[ *fndx ][ *vndx ] = malloc( (size_t) size ) ;
        bptr = (FREAL *)baddr[ *fndx ][ *vndx ] ;

        if ( ! bptr )    /** CHECK FOR MALLOC() FAILURE **/
            {
            m3mesgc( "Error allocating internal buffer for BUFVGTP3()" ) ;
            return( 0 ) ;
            }                   /**  END IF-BLOCK:  MALLOC() FAILED **/

        }           /**  END IF-BLOCK:  NEED TO ALLOCATE THIS VARIABLE **/

    if ( *rflag )                               /** READ IS NECESSARY **/
        {
        bptr += ( *bsize ) * ( *delta ) ;        /**  = OFFSET FOR READ **/
        ierr = RDVARS( fndx, vndx, dims, delt, bsize, (void *)bptr ) ;
        if ( ! ierr )  
            {
            sprintf( mesg, "netCDF error %d reading file", ierr ) ;
            m3mesgc( mesg ) ;
            return( 0 ) ;
            }
        }                       /**  END IF-BLOCK:  READ NECESSARY **/

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION bufvgt3() **/



/*****************  BUFVGT3D: *****************************************/
/** Read data for INTERP3() and DDTVAR3() (type double) **/
    
FINT BUFVGT3D( FINT  *fndx,      /** M3 file index **/
               FINT  *vndx,      /** M3 variable index **/
               FINT  *rflag,     /** read-only flag **/
               FINT   dims[],    /** hyperslab corner   arg for NCVGT() **/
               FINT   delt[],    /** hyperslab diagonal arg for NCVGT() **/
               FINT  *bsize,     /** buffer size **/
               FINT  *delta,     /** offset for read-op **/
               FINT  *tstep )    /** TRUE iff time-dependent file **/

    {  /**  begin body of bufvgt3d() **/

    FINT    size ;               /** SCRATCH VARIABLE:  BUFFER SIZE  **/
    FINT    ierr ;               /** RETURN FLAG FOR NCVGT()  **/
    double *bptr ;               /** FOR TRAVERSING INTERPOLATION BUFFER **/
    char    mesg[ 81 ] ;

    if ( ! ( bptr = (double *) baddr[ *fndx ][ *vndx ] ) )   /** NOT YET ALLOCATED **/
        {
        size = ( *tstep ? 2 : 1 ) * sizeof( double ) * (*bsize );
        baddr[ *fndx ][ *vndx ] = malloc( (size_t) size ) ;
        bptr = (double *) baddr[ *fndx ][ *vndx ];

        if ( ! bptr )    /** CHECK FOR MALLOC() FAILURE **/
            {
            m3mesgc( "Error allocating internal buffer for BUFVGT3D()" ) ;
            return( 0 ) ;
            }                   /**  END IF-BLOCK:  MALLOC() FAILED **/

        }           /**  END IF-BLOCK:  NEED TO ALLOCATE THIS VARIABLE **/

    if ( *rflag )                               /** READ IS NECESSARY **/
        {
        bptr += ( *bsize ) * ( *delta ) ;        /**  = OFFSET FOR READ **/
        ierr = RDVARS( fndx, vndx, dims, delt, bsize, (void *)bptr ) ;
        if ( ! ierr )  
            {
            sprintf( mesg, "netCDF error %d reading file", ierr ) ;
            m3mesgc( mesg ) ;
            return( 0 ) ;
            }
        }                       /**  END IF-BLOCK:  READ NECESSARY **/

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION bufvgt3() **/



#ifdef IOAPICPL

#include "state3.h"

extern IOAPI_BSTATE3 BSTATE3;
extern IOAPI_CSTATE3 CSTATE3;

/*****************  BUFVRD3: *****************************************/
/** Read virtual-file data for INTERP3() and DDTVAR3() (type FREAL) **/
    
FINT BUFVRD3 ( FINT  *fndx,      /** M3 file index **/
               FINT  *vndx,      /** M3 variable index **/
               FINT  *delta,     /** subscript offset  for read-op **/
               FINT  *bsize,     /** buffer size **/
               FINT  *kdate,     /** date yyyyddd **/
               FINT  *ktime      /** time hhmmss **/
               )

    {  /**  begin body of bufint3() **/

    FINT   size ;               /** SCRATCH VARIABLE:  BUFFER SIZE  **/
    FINT   ierr ;               /** RETURN FLAG FOR NCVGT()  **/
    FREAL *bptr ;               /** FOR TRAVERSING INTERPOLATION BUFFER **/
    char   mesg[ 81 ] ;
    FINT   fid, skip, istep ;

    fid   = (FINT) (*fndx - 1) ;
    istep = BSTATE3.tstep[ fid ] ;
    if ( ! ( bptr = (FREAL *)baddr[ *fndx ][ *vndx ] ) )   /** NOT YET ALLOCATED **/
        {
        size = (FINT)( ( istep ? 2 : 1 ) * sizeof( FREAL ) * (*bsize ) );
        baddr[ *fndx ][ *vndx ] = malloc( (size_t) size ) ;
        bptr = (FREAL *)baddr[ *fndx ][ *vndx ] ;

        if ( ! bptr )    /** CHECK FOR MALLOC() FAILURE **/
            {
            m3mesgc( "Error allocating internal buffer for BUFVRD3()" ) ;
            return( 0 ) ;
            }                   /**  END IF-BLOCK:  MALLOC() FAILED **/

        }           /**  END IF-BLOCK:  NEED TO ALLOCATE THIS VARIABLE **/

    bptr += ( *bsize ) * ( *delta ) ;        /**  = OFFSET FOR READ **/
    skip  = (FINT) 0 ;
    ierr  =  READ3V( fndx, vndx, (FINT *) & skip, bsize,
                     kdate, ktime, (void *)bptr );
    if ( ! ierr )  
        {
        sprintf( mesg, "PVM error %d reading file", ierr ) ;
        m3mesgc( mesg ) ;
        return( 0 ) ;
        }

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION bufvrd3() **/


/*****************  BUFVRD3D: *****************************************/
/** Read virtual-file data for INTERP3() and DDTVAR3() (type FREAL) **/
    
FINT BUFVRD3D ( FINT  *fndx,      /** M3 file index **/
                FINT  *vndx,      /** M3 variable index **/
                FINT  *delta,     /** subscript offset  for read-op **/
                FINT  *bsize,     /** buffer size **/
                FINT  *kdate,     /** date yyyyddd **/
                FINT  *ktime      /** time hhmmss **/
                )

    {  /**  begin body of bufint3() **/

    FINT    size ;               /** SCRATCH VARIABLE:  BUFFER SIZE  **/
    FINT    ierr ;               /** RETURN FLAG FOR NCVGT()  **/
    double *bptr ;               /** FOR TRAVERSING INTERPOLATION BUFFER **/
    char    mesg[ 81 ] ;
    FINT    fid, skip, istep ;

    fid   = (FINT) (*fndx - 1) ;
    istep = BSTATE3.tstep[ fid ] ;
    if ( ! ( bptr = (double *)baddr[ *fndx ][ *vndx ] ) )   /** NOT YET ALLOCATED **/
        {
        size = (FINT)( ( istep ? 2 : 1 ) * sizeof( double ) * (*bsize ) );
        baddr[ *fndx ][ *vndx ] = malloc( (size_t) size ) ;
        bptr = (double *)baddr[ *fndx ][ *vndx ] ;

        if ( ! bptr )    /** CHECK FOR MALLOC() FAILURE **/
            {
            m3mesgc( "Error allocating internal buffer for BUFVRD3D3()" ) ;
            return( 0 ) ;
            }                   /**  END IF-BLOCK:  MALLOC() FAILED **/

        }           /**  END IF-BLOCK:  NEED TO ALLOCATE THIS VARIABLE **/

    bptr += ( *bsize ) * ( *delta ) ;        /**  = OFFSET FOR READ **/
    skip  = (FINT) 0 ;
    ierr  =  READ3V( fndx, vndx, (FINT *) & skip, bsize,
                     kdate, ktime, (void *)bptr );
    if ( ! ierr )  
        {
        sprintf( mesg, "PVM error %d reading file", ierr ) ;
        m3mesgc( mesg ) ;
        return( 0 ) ;
        }

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION bufvrd3d() **/


#endif          /**  ifdef IOAPICPL  **/


/*****************  BUFINT3: *****************************************/
/** Do time interpolation for INTERP3() (type FREAL) **/
    
FINT BUFINT3 ( FINT  *fndx,      /** M3 file index **/
               FINT  *vndx,      /** M3 variable index **/
               FINT  *bsize,     /** buffer size **/
               FINT  *last,      /**   " for p coeff in interpolation **/
               FINT  *tstep,     /** TRUE iff time-dependent file **/
               FREAL *p,         /** interpolation coeff for last **/
               FREAL *q,         /** ... for (1 - last) **/
               FREAL *buffer )   /** output buffer array **/

    {  /**  begin body of bufint3() **/

    FINT   i ;                  /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT   size  ;              /** SCRATCH VARIABLE:  BUFFER SIZE  **/
    FINT   size4 ;              /** SCRATCH VARIABLE:  SIZE%4  **/
    FREAL *bptr, *pptr, *qptr ; /** FOR TRAVERSING INTERPOLATION BUFFER **/
    FREAL  pp , qq ;            /** COEFFS AT P,Q **/

    if ( ! ( bptr = (FREAL *)baddr[ *fndx ][ *vndx ] ) )   /** NOT YET ALLOCATED **/
        {
        m3mesgc( "Error referencing internal buffer for INTERP3()" ) ;
        return( 0 ) ;
        }           /**  END IF-BLOCK:  NEED TO ALLOCATE THIS VARIABLE **/

    size  = *bsize ;             /** = SIZEOF( TIME-STEP RECORD ) **/
    size4 = size % 4 ;

    if ( *tstep )        /** IF TIME-STEPPED FILE **/
        {
        pp   = *p ;      /** BY CONSTRUCTION:  pp + qq = 1; pp,qq >= 0 **/
        qq   = *q ;
        if ( qq == 0.0 ) 
            {
            pptr = bptr + ( *last ? size : 0 ) ;
            for ( i = 0 ; i < size4 ; i++ )
                {
                buffer[ i ] = pptr[ i ] ;
                }                   /**  END REMAINDER-LOOP ON I:  COPY BUFFER **/
            for (       ; i < size ; i+=4 )
                {
                buffer[ i   ] = pptr[ i   ] ;
                buffer[ i+1 ] = pptr[ i+1 ] ;
                buffer[ i+2 ] = pptr[ i+2 ] ;
                buffer[ i+3 ] = pptr[ i+3 ] ;
                }                   /**  END FOR-LOOP ON I:  COPY BUFFER **/
            }                       /** IF TIME-STEPPED FILE OR NOT **/
        else if ( pp == 0.0 ) 
            {
            qptr = bptr + ( *last ? 0 : size ) ;
            for ( i = 0 ; i < size4 ; i++ )
                {
                buffer[ i ] = qptr[ i ] ;
                }                   /**  END REMAINDER-LOOP ON I:  COPY BUFFER **/
            for (      ; i < size ; i+=4 )
                {
                buffer[ i   ] = qptr[ i   ] ;
                buffer[ i+1 ] = qptr[ i+1 ] ;
                buffer[ i+2 ] = qptr[ i+2 ] ;
                buffer[ i+3 ] = qptr[ i+3 ] ;
                }                   /**  END FOR-LOOP ON I:  COPY BUFFER **/
            }                       /** IF TIME-STEPPED FILE OR NOT **/
        else{
            pptr = bptr + ( *last ? size : 0 ) ;
            qptr = bptr + ( *last ? 0 : size ) ;
            for ( i = 0 ; i < size4 ; i++ )
                {
                buffer[ i ] = pp * pptr[ i ]  +  qq * qptr[ i ] ;
                }           /**  END REMAINDER-LOOP ON I:  INTERPOLATE FROM BUFFER **/
            for (       ; i < size ; i++ )
                {
                buffer[ i   ] = pp * pptr[ i   ]  +  qq * qptr[ i   ] ;
                }           /**  END FOR-LOOP ON I:  INTERPOLATE FROM BUFFER **/
            }
        }			/** END IF:  qq==0, or pp==0, OR NOT **/
    else                                /** ELSE TIME-INDEPENDENT FILE **/
        {
        pptr = bptr ;
        for ( i = 0 ; i < size4 ; i++ )
            {
            buffer[ i ] = pptr[ i ] ;
            }                   /**  END REMAINDER-LOOP ON I:  COPY BUFFER **/
        for (       ; i < size ; i+=4 )
            {
            buffer[ i   ] = pptr[ i   ] ;
            buffer[ i+1 ] = pptr[ i+1 ] ;
            buffer[ i+2 ] = pptr[ i+2 ] ;
            buffer[ i+3 ] = pptr[ i+3 ] ;
            }                   /**  END FOR-LOOP ON I:  COPY BUFFER **/
        }                       /** IF TIME-STEPPED FILE OR NOT **/

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION bufint3() **/



/*****************  BUFINT3D: *****************************************/
/** Do time interpolation for INTERP3() (type double) **/
    
FINT BUFINT3D( FINT   *fndx,      /** M3 file index **/
               FINT   *vndx,      /** M3 variable index **/
               FINT   *bsize,     /** buffer size **/
               FINT   *last,      /**   " for p coeff in interpolation **/
               FINT   *tstep,     /** TRUE iff time-dependent file **/
               double *p,         /** interpolation coeff for last **/
               double *q,         /** ... for (1 - last) **/
               double *buffer )   /** output buffer array **/

    {  /**  begin body of bufint3() **/

    FINT   i ;                   /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT   size ;                /** SCRATCH VARIABLE:  BUFFER SIZE  **/
    FINT   size4 ;               /** SCRATCH VARIABLE:  remainder  **/
    double *bptr, *pptr, *qptr ; /** FOR TRAVERSING INTERPOLATION BUFFER **/
    double  pp , qq ;            /** COEFFS AT P,Q **/

    if ( ! ( bptr = (double *) baddr[ *fndx ][ *vndx ] ) )   /** NOT YET ALLOCATED **/
        {
        m3mesgc( "Error referencing internal buffer for INTERP3()" ) ;
        return( 0 ) ;
        }           /**  END IF-BLOCK:  NEED TO ALLOCATE THIS VARIABLE **/

    size  = *bsize ;             /** = SIZEOF( TIME-STEP RECORD ) **/
    size4 = size % 4 ;

    if ( *tstep )        /** IF TIME-STEPPED FILE **/
        {
        pp   = *p ;      /** BY CONSTRUCTION:  pp + qq = 1; pp,qq >= 0 **/
        qq   = *q ;
        if ( qq == 0.0 ) 
            {
            pptr = bptr + ( *last ? size : 0 ) ;
            for ( i = 0 ; i < size4 ; i++ )
                {
                buffer[ i ] = pptr[ i ] ;
                }                   /**  END REMAINDER-LOOP ON I:  COPY BUFFER **/
            for (       ; i < size ; i+=4 )
                {
                buffer[ i   ] = pptr[ i   ] ;
                buffer[ i+1 ] = pptr[ i+1 ] ;
                buffer[ i+2 ] = pptr[ i+2 ] ;
                buffer[ i+3 ] = pptr[ i+3 ] ;
                }                   /**  END FOR-LOOP ON I:  COPY BUFFER **/
            }                       /** IF TIME-STEPPED FILE OR NOT **/
        else if ( pp == 0.0 ) 
            {
            qptr = bptr + ( *last ? 0 : size ) ;
            for ( i = 0 ; i < size4 ; i++ )
                {
                buffer[ i ] = qptr[ i ] ;
                }                   /**  END REMAINDER-LOOP ON I:  COPY BUFFER **/
            for (      ; i < size ; i+=4 )
                {
                buffer[ i   ] = qptr[ i   ] ;
                buffer[ i+1 ] = qptr[ i+1 ] ;
                buffer[ i+2 ] = qptr[ i+2 ] ;
                buffer[ i+3 ] = qptr[ i+3 ] ;
                }                   /**  END FOR-LOOP ON I:  COPY BUFFER **/
            }                       /** IF TIME-STEPPED FILE OR NOT **/
        else{
            pptr = bptr + ( *last ? size : 0 ) ;
            qptr = bptr + ( *last ? 0 : size ) ;
            for ( i = 0 ; i < size4 ; i++ )
                {
                buffer[ i ] = pp * pptr[ i ]  +  qq * qptr[ i ] ;
                }           /**  END REMAINDER-LOOP ON I:  INTERPOLATE FROM BUFFER **/
            for (       ; i < size ; i+=4 )
                {
                buffer[ i   ] = pp * pptr[ i   ]  +  qq * qptr[ i   ] ;
                buffer[ i+1 ] = pp * pptr[ i+1 ]  +  qq * qptr[ i+1 ] ;
                buffer[ i+2 ] = pp * pptr[ i+2 ]  +  qq * qptr[ i+2 ] ;
                buffer[ i+3 ] = pp * pptr[ i+3 ]  +  qq * qptr[ i+3 ] ;
                }           /**  END FOR-LOOP ON I:  INTERPOLATE FROM BUFFER **/
            }
        }			/** END IF:  qq==0, or pp==0, OR NOT **/
    else                                /** ELSE TIME-INDEPENDENT FILE **/
        {
        pptr = bptr ;
        for ( i = 0 ; i < size4 ; i++ )
            {
            buffer[ i ] = pptr[ i ] ;
            }                   /**  END REMAINDER-LOOP ON I:  COPY BUFFER **/
        for (       ; i < size ; i+=4 )
            {
            buffer[ i   ] = pptr[ i   ] ;
            buffer[ i+1 ] = pptr[ i+1 ] ;
            buffer[ i+2 ] = pptr[ i+2 ] ;
            buffer[ i+3 ] = pptr[ i+3 ] ;
            }                   /**  END FOR-LOOP ON I:  COPY BUFFER **/
        }                       /** IF TIME-STEPPED FILE OR NOT **/

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION bufint3d() **/



/*****************  BUFCRE3: *****************************************/
/** CREATE / SET UP INTERNAL BUFFERS FOR WRITE3 **/    

FINT BUFCRE3 ( FINT  *fndx,     /** M3 file index **/
               FINT  *nvars,    /** number of variables **/
               FINT  *nlays,    /** number of layers **/
               FINT  *bsize,    /** individual-variable/layer buffer size **/
               FINT   btype[],  /** individual-variable buffer types **/
               FINT  *tstep )   /** time step or 0 (time-independent data) **/

    {  /**  begin body of bufcre3() **/

    FINT   i  ;                        /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT   rsize  ;                    /** RECORD-SIZE **/
    FINT   asize  ;

    rsize = ( *bsize ) * ( *nlays ) * ( *tstep ? 2 : 1 ) ;
    
    
    if ( ! baddr[ *fndx ][ 1 ] )   /** "FILE" NOT YET ALLOCATED **/
        {
                                   /** SET UP EACH VARIABLE'S ADDRESS **/
        for ( i=1 ;  i <= (*nvars) ;  i++ )
            {
            if      ( btype[i] == M3REAL ) asize = rsize * sizeof( FREAL ) ;
            else if ( btype[i] == M3DBLE ) asize = rsize * sizeof( double ) ;
            else if ( btype[i] == M3INT  ) asize = rsize * sizeof( FINT ) ;
            else if ( btype[i] == M3INT8 ) asize = rsize * sizeof( int64_t ) ;
            if ( ! ( baddr[ *fndx ][ i ] = malloc( (size_t) asize ) ) )
                {
                m3mesgc( "Error allocating internal buffer for BUFCRE3()" ) ;
                return( 0 ) ;
                }                     /**  END IF-BLOCK:  MALLOC() FAILED **/
            }           /** END LOOP SETTING baddr[][] FOR THIS VARIABLE**/
        }               /**  END IF-BLOCK:  NEED TO ALLOCATE THIS "FILE" **/

    return( -1 ) ;          /** .TRUE. **/

    }           /**  END FUNCTION BUFCRE3() **/



/*****************  BUFPUT3: *****************************************/
/** WRITE TO INTERNAL BUFFERS FOR WRITE3 (type FREAL) **/    

FINT BUFPUT3 ( FINT  *fndx,     /** M3 file index **/
               FINT  *vndx,     /** variable index **/
               FINT  *bsize,    /** individual-variable buffer size **/
               FINT  *where,    /** 0,1 subscript into circ buffer for WRITE **/
               FINT  *tstep,    /** TRUE iff time-stepped data **/
               FREAL *buffer )  /** output buffer array **/

    {  /**  begin body of bufput3() **/

    FINT   j ;                         /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT   size ;                      /** TIMESTEP-RECORD SIZE **/
    FREAL *bptr, *pptr, *qptr ;        /** FOR TRAVERSING BUFFERS **/

    size = ( *bsize ) ;
    
    if ( !( bptr = (FREAL *)baddr[ *fndx ][ *vndx ] ) ) /** "FILE" NOT YET ALLOCATED **/
        {
        size = (FINT)( ( *tstep ? 2 : 1 ) * sizeof( FREAL ) * (*bsize ) );
        baddr[ *fndx ][ *vndx ] = malloc( (size_t) size ) ;
        bptr = (FREAL *)baddr[ *fndx ][ *vndx ] ;

        if ( ! bptr )    /** CHECK FOR MALLOC() FAILURE **/
            {
            m3mesgc( "Error allocating internal buffer for BUFPUT3()" ) ;
            return( 0 ) ;
            }                   /**  END IF-BLOCK:  MALLOC() FAILED **/

        }

    if ( *where ) bptr += size ;
    for  ( j = 0 , pptr = bptr , qptr = buffer ;
           j < size ;
           j++ , pptr++ , qptr++ )
        {
        *pptr = *qptr ;
        }               /**  END FOR-LOOP COPYING THIS VARIABLE **/

    return( -1 ) ;          /** .TRUE. **/

    }           /**  END FUNCTION BUFPUT3() **/



/*****************  BUFPUT3I: *****************************************/
/** WRITE TO INTERNAL BUFFERS FOR WRITE3 ( type int) **/    

FINT BUFPUT3I( FINT  *fndx,     /** M3 file index **/
               FINT  *vndx,     /** variable index **/
               FINT  *bsize,    /** individual-variable buffer size **/
               FINT  *where,    /** 0,1 subscript into circ buffer for WRITE **/
               FINT  *tstep,    /** TRUE iff time-stepped data **/
               FINT  *buffer )  /** output buffer array **/

    {  /**  begin body of bufput3() **/

    FINT   j ;                         /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT   size ;                      /** TIMESTEP-RECORD SIZE **/
    FINT  *bptr, *pptr, *qptr ;        /** FOR TRAVERSING BUFFERS **/

    size = ( *bsize ) ;
    
    if ( !( bptr = (FINT *) baddr[ *fndx ][ *vndx ] ) ) /** "FILE" NOT YET ALLOCATED **/
        {
        size = (FINT)( ( *tstep ? 2 : 1 ) * sizeof( FINT ) * (*bsize ) );
        baddr[ *fndx ][ *vndx ] = malloc( (size_t) size ) ;
        bptr = (FINT *)baddr[ *fndx ][ *vndx ] ;

        if ( ! bptr )    /** CHECK FOR MALLOC() FAILURE **/
            {
            m3mesgc( "Error allocating internal buffer for BUFPUT3I()" ) ;
            return( 0 ) ;
            }                   /**  END IF-BLOCK:  MALLOC() FAILED **/

        }

    if ( *where ) bptr += size ;
    for  ( j = 0 , pptr = bptr , qptr = buffer ;
           j < size ;
           j++ , pptr++ , qptr++ )
        {
        *pptr = *qptr ;
        }               /**  END FOR-LOOP COPYING THIS VARIABLE **/

    return( -1 ) ;          /** .TRUE. **/

    }           /**  END FUNCTION BUFPUT3I() **/



/*****************  BUFPUT3D: *****************************************/
/** WRITE TO INTERNAL BUFFERS FOR WRITE3 (type double) **/    

FINT BUFPUT3D( FINT   *fndx,     /** M3 file index **/
               FINT   *vndx,     /** variable index **/
               FINT   *bsize,    /** individual-variable buffer size **/
               FINT   *where,    /** 0,1 subscript into circ buffer for WRITE **/
               FINT  *tstep,    /** TRUE iff time-stepped data **/
               double *buffer )  /** output buffer array **/

    {  /**  begin body of bufput3() **/

    FINT    j ;                         /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT    size ;                      /** TIMESTEP-RECORD SIZE **/
    double *bptr, *pptr, *qptr ;        /** FOR TRAVERSING BUFFERS **/

    size = ( *bsize ) ;
    
    if ( !( bptr = (double *) baddr[ *fndx ][ *vndx ] ) ) /** "FILE" NOT YET ALLOCATED **/
        {
        size = ( *tstep ? 2 : 1 ) * sizeof( double ) * (*bsize );
        baddr[ *fndx ][ *vndx ] = malloc( (size_t) size ) ;
        bptr = (double *) baddr[ *fndx ][ *vndx ];

        if ( ! bptr )    /** CHECK FOR MALLOC() FAILURE **/
            {
            m3mesgc( "Error allocating internal buffer for BUFPUT3D()" ) ;
            return( 0 ) ;
            }                   /**  END IF-BLOCK:  MALLOC() FAILED **/

        }

    if ( *where ) bptr += size ;
    for  ( j = 0 , pptr = bptr , qptr = buffer ;
           j < size ;
           j++ , pptr++ , qptr++ )
        {
        *pptr = *qptr ;
        }               /**  END FOR-LOOP COPYING THIS VARIABLE **/

    return( -1 ) ;          /** .TRUE. **/

    }           /**  END FUNCTION BUFPUT3D() **/



/*****************  BUFGET3: *****************************************/
/** READ TO INTERNAL BUFFERS FOR READ3() **/

FINT BUFGET3 ( FINT  *fndx,     /** M3 file     index **/
               FINT  *vndx,     /** M3 variable index **/
               FINT  *lndx,     /** M3 layer    index **/
               FINT  *nlays,    /** number of layers  **/
               FINT  *bsize,    /** individual-variable buffer size **/
               FINT  *where,    /** 0,1 subscript into circ buffer for WRITE **/
               FREAL *buffer )  /** output buffer array **/

    {  /**  begin body of bufget3() **/

    int    size , vsiz  ;        /** SCRATCH VARIABLES:  BUFFER SIZE  **/
    FREAL *bptr, *pptr, *qptr ;  /** FOR TRAVERSING BUFFERS **/

    if ( !( bptr = (FREAL *)baddr[ *fndx ][ *vndx ] ) ) 
        {
        m3mesgc( "BUFFERED file not yet allocated" ) ;
        return( 0 ) ;
        }
    
    size = ( *bsize ) ;			/** SIZE OF ONE LAYER  **/
    vsiz = size * ( *nlays ) ;		/** SIZE OF ENTIRE VBLE **/
    if ( *lndx > 0 )
        {
        bptr += ( size * ( *lndx - 1 ) ) ;  /** OFFSET FOR REQUESTED LAYER **/
        }
    else{
        size = vsiz ;			    /** READ ENTIRE VBLE **/
        }
    if ( *where ) bptr += vsiz ;
    
    for  ( pptr = bptr , qptr = buffer ;
           pptr < bptr + size ;
           pptr++, qptr++ )
        {
        *qptr = *pptr ;
        }                   /**  END FOR-LOOP COPYING THIS VARIABLE **/
    
    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION BUFGET3() **/


/*****************  BUFGET3I: *****************************************/
/** READ TO INTERNAL BUFFERS FOR READ3() **/

FINT BUFGET3I( FINT  *fndx,     /** M3 file     index **/
               FINT  *vndx,     /** M3 variable index **/
               FINT  *lndx,     /** M3 layer    index **/
               FINT  *nlays,    /** number of layers  **/
               FINT  *bsize,    /** individual-variable buffer size **/
               FINT  *where,    /** 0,1 subscript into circ buffer for WRITE **/
               FINT  *buffer )  /** output buffer array **/

    {  /**  begin body of bufget3() **/

    FINT   size , vsiz  ;        /** SCRATCH VARIABLES:  BUFFER SIZE  **/
    FINT  *bptr, *pptr, *qptr ;  /** FOR TRAVERSING BUFFERS **/

    if ( !( bptr = (FINT *) baddr[ *fndx ][ *vndx ] ) ) 
        {
        m3mesgc( "BUFFERED file not yet allocated" ) ;
        return( 0 ) ;
        }
    
    size = ( *bsize ) ;			/** SIZE OF ONE LAYER  **/
    vsiz = size * ( *nlays ) ;		/** SIZE OF ENTIRE VBLE **/
    if ( *lndx > 0 )
        {
        bptr += ( size * ( *lndx - 1 ) ) ;  /** OFFSET FOR REQUESTED LAYER **/
        }
    else{
        size = vsiz ;			    /** READ ENTIRE VBLE **/
        }
    if ( *where ) bptr += vsiz ;
    
    for  ( pptr = bptr , qptr = buffer ;
           pptr < bptr + size ;
           pptr++, qptr++ )
        {
        *qptr = *pptr ;
        }                   /**  END FOR-LOOP COPYING THIS VARIABLE **/
    
    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION BUFGET3I() **/


/*****************  BUFGET3D: *****************************************/
/** READ TO INTERNAL BUFFERS FOR READ3() (type double) **/

FINT BUFGET3D( FINT   *fndx,     /** M3 file     index **/
               FINT   *vndx,     /** M3 variable index **/
               FINT   *lndx,     /** M3 layer    index **/
               FINT   *nlays,    /** number of layers  **/
               FINT   *bsize,    /** individual-variable buffer size **/
               FINT   *where,    /** 0,1 subscript into circ buffer for WRITE **/
               double *buffer )  /** output buffer array **/

    {  /**  begin body of bufget3() **/

    FINT    size , vsiz  ;        /** SCRATCH VARIABLES:  BUFFER SIZE  **/
    double *bptr, *pptr, *qptr ;  /** FOR TRAVERSING BUFFERS **/

    if ( !( bptr = (double *)baddr[ *fndx ][ *vndx ] ) ) 
        {
        m3mesgc( "BUFFERED file not yet allocated" ) ;
        return( 0 ) ;
        }
    
    size = ( *bsize ) ;			/** SIZE OF ONE LAYER  **/
    vsiz = size * ( *nlays ) ;		/** SIZE OF ENTIRE VBLE **/
    if ( *lndx > 0 )
        {
        bptr += ( size * ( *lndx - 1 ) ) ;  /** OFFSET FOR REQUESTED LAYER **/
        }
    else{
        size = vsiz ;			    /** READ ENTIRE VBLE **/
        }
    if ( *where ) bptr += vsiz ;
    
    for  ( pptr = bptr , qptr = buffer ;
           pptr < bptr + size ;
           pptr++, qptr++ )
        {
        *qptr = *pptr ;
        }                   /**  END FOR-LOOP COPYING THIS VARIABLE **/
    
    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION BUFGET3D() **/


/*****************  BUFXTR3: *****************************************/
/** READ TO INTERNAL BUFFERS FOR XTRACT3() **/

FINT BUFXTR3 ( FINT  *fndx,     /** M3 file      index **/
               FINT  *vndx,     /** M3 variable  index **/
               FINT  *lolay,    /** lower layer  index for XTRACT window **/
               FINT  *hilay,    /** upper layer  index **/
               FINT  *lorow,    /** lower row    index **/
               FINT  *hirow,    /** upper row    index **/
               FINT  *locol,    /** lower column index **/
               FINT  *hicol,    /** upper column index **/
               FINT  *nlays,    /** number of layers   **/
               FINT  *nrows,    /** number of rows     **/
               FINT  *ncols,    /** number of columns  **/
               FINT  *where,    /** 0,1 subscript into circ buffer for XTRACT **/
               FREAL *buffer )  /** output buffer array **/

    {  /**  begin body of bufget3() **/

    FINT   c , r , l ;                /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT   nc, nr, nl ;
    FINT   loc, hic, lor, hir, lol, hil ;
    FREAL *bptr, *qptr ;

    if ( !( bptr = (FREAL *)baddr[ *fndx ][ *vndx ] ) ) 
        {
        m3mesgc( "BUFFERED file not yet allocated." ) ;
        return( 0 ) ;
        }
    
    nc = *ncols ;
    nr = *nrows ;
    nl = *nlays ;
    
    if ( *where ) bptr += ( nc * nr * nl ) ;
    
    loc = *locol - 1 ;  /** compensate for Fortran conventions      **/
    hic = *hicol ;      /** (1-based subscripting) on window bounds **/
    lor = *lorow - 1 ;  /** from calling routine XTRACT3()          **/
    hir = *hirow ;
    lol = *lolay - 1 ;
    hil = *hilay ;
    
    qptr = buffer ;
    
    for  ( l = lol ; l < hil ; l++ )
    for  ( r = lor ; r < hir ; r++ )
    for  ( c = loc ; c < hic ; c++, qptr++ )
        {
        *qptr = VALXTR3( l,r,c ) ;
        }                   /**  END FOR-LOOP-NEST COPYING THIS VARIABLE **/
    

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION BUFXTR3() **/


/*****************  BUFXTR3I: *****************************************/
/** READ TO INTERNAL BUFFERS FOR XTRACT3() (type integer) **/

FINT BUFXTR3I( FINT  *fndx,     /** M3 file      index **/
               FINT  *vndx,     /** M3 variable  index **/
               FINT  *lolay,    /** lower layer  index for XTRACT window **/
               FINT  *hilay,    /** upper layer  index **/
               FINT  *lorow,    /** lower row    index **/
               FINT  *hirow,    /** upper row    index **/
               FINT  *locol,    /** lower column index **/
               FINT  *hicol,    /** upper column index **/
               FINT  *nlays,    /** number of layers   **/
               FINT  *nrows,    /** number of rows     **/
               FINT  *ncols,    /** number of columns  **/
               FINT  *where,    /** 0,1 subscript into circ buffer for XTRACT **/
               FINT  *buffer )  /** output buffer array **/

    {  /**  begin body of bufget3() **/

    FINT   c , r , l ;                /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT   nc, nr, nl ;
    FINT   loc, hic, lor, hir, lol, hil ;
    FINT  *bptr, *qptr ;

    if ( !( bptr = (FINT *) baddr[ *fndx ][ *vndx ] ) ) 
        {
        m3mesgc( "BUFFERED file not yet allocated." ) ;
        return( 0 ) ;
        }
    
    nc = *ncols ;
    nr = *nrows ;
    nl = *nlays ;
    
    if ( *where ) bptr += ( nc * nr * nl ) ;
    
    loc = *locol - 1 ;  /** compensate for Fortran conventions      **/
    hic = *hicol ;      /** (1-based subscripting) on window bounds **/
    lor = *lorow - 1 ;  /** from calling routine XTRACT3()          **/
    hir = *hirow ;
    lol = *lolay - 1 ;
    hil = *hilay ;
    
    qptr = buffer ;
    
    for  ( l = lol ; l < hil ; l++ )
    for  ( r = lor ; r < hir ; r++ )
    for  ( c = loc ; c < hic ; c++, qptr++ )
        {
        *qptr = VALXTR3( l,r,c ) ;
        }                   /**  END FOR-LOOP-NEST COPYING THIS VARIABLE **/
    

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION BUFXTR3I() **/


/*****************  BUFXTR3: *****************************************/
/** READ TO INTERNAL BUFFERS FOR XTRACT3() (type double)**/

FINT BUFXTR3D( FINT   *fndx,     /** M3 file      index **/
               FINT   *vndx,     /** M3 variable  index **/
               FINT   *lolay,    /** lower layer  index for XTRACT window **/
               FINT   *hilay,    /** upper layer  index **/
               FINT   *lorow,    /** lower row    index **/
               FINT   *hirow,    /** upper row    index **/
               FINT   *locol,    /** lower column index **/
               FINT   *hicol,    /** upper column index **/
               FINT   *nlays,    /** number of layers   **/
               FINT   *nrows,    /** number of rows     **/
               FINT   *ncols,    /** number of columns  **/
               FINT   *where,    /** 0,1 subscript into circ buffer for XTRACT **/
               double *buffer )  /** output buffer array **/

    {  /**  begin body of bufget3() **/

    FINT   c , r , l ;                /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT   nc, nr, nl ;
    FINT   loc, hic, lor, hir, lol, hil ;
    double *bptr, *qptr ;

    if ( !( bptr = (double *) baddr[ *fndx ][ *vndx ] ) ) 
        {
        m3mesgc( "BUFFERED file not yet allocated." ) ;
        return( 0 ) ;
        }
    
    nc = *ncols ;
    nr = *nrows ;
    nl = *nlays ;
    
    if ( *where ) bptr += ( nc * nr * nl ) ;
    
    loc = *locol - 1 ;  /** compensate for Fortran conventions      **/
    hic = *hicol ;      /** (1-based subscripting) on window bounds **/
    lor = *lorow - 1 ;  /** from calling routine XTRACT3()          **/
    hir = *hirow ;
    lol = *lolay - 1 ;
    hil = *hilay ;
    
    qptr = buffer ;
    
    for  ( l = lol ; l < hil ; l++ )
    for  ( r = lor ; r < hir ; r++ )
    for  ( c = loc ; c < hic ; c++, qptr++ )
        {
        *qptr = VALXTR3( l,r,c ) ;
        }                   /**  END FOR-LOOP-NEST COPYING THIS VARIABLE **/
    

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION BUFXTR3D() **/


/*****************  BUFDDT3: *****************************************/
/** Do derivative calculation for DDTVAR3() (type FREAL)**/
    
FINT BUFDDT3 ( FINT  *fndx,      /** M3 file index **/
               FINT  *vndx,      /** M3 variable index **/
               FINT  *bsize,     /** buffer size **/
               FINT  *last,      /**   " for p coeff in interpolation **/
               FINT  *tstep,     /** TRUE iff time-dependent file **/
               FREAL *p,         /** 1.0 / timestep **/
               FREAL *buffer )   /** output buffer array **/

    {  /**  begin body of bufint3() **/

    FINT   i ;                  /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT   size ;               /** SCRATCH VARIABLE:  BUFFER SIZE  **/
    FREAL *bptr, *pptr, *qptr ; /** FOR TRAVERSING INTERPOLATION BUFFER **/
    FREAL  pp ;                 /** INVERSE TIMESTEP **/

    if ( ! ( bptr = (FREAL *)baddr[ *fndx ][ *vndx ] ) )   /** NOT YET ALLOCATED **/
        {
        m3mesgc( "Error referencing internal buffer for DDTVAR3()" ) ;
        return( 0 ) ;
        }           /**  END IF-BLOCK:  NEED TO ALLOCATE THIS VARIABLE **/

    size = *bsize ;             /** = SIZEOF( TIME-STEP RECORD ) **/

    for  ( i    = 0 ,
           pptr = bptr + ( *last ? size : 0 ) ,
           qptr = bptr + ( *last ? 0 : size ) ,
           pp   = *p ;
           i < size ;
           i++ )
        {
        buffer[ i ] = pp * ( qptr[ i ]  -  pptr[ i ] ) ;
        }           /**  END FOR-LOOP ON I:  DDT FROM BUFFER **/

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION BUFDDT3() **/



/*****************  BUFDDT3D: *****************************************/
/** Do derivative calculation for DDTVAR3() (type double)**/
    
FINT BUFDDT3D( FINT   *fndx,      /** M3 file index **/
               FINT   *vndx,      /** M3 variable index **/
               FINT   *bsize,     /** buffer size **/
               FINT   *last,      /**   " for p coeff in interpolation **/
               FINT   *tstep,     /** TRUE iff time-dependent file **/
               double *p,         /** 1.0 / timestep **/
               double *buffer )   /** output buffer array **/

    {  /**  begin body of bufint3() **/

    FINT   i ;                  /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT   size ;               /** SCRATCH VARIABLE:  BUFFER SIZE  **/
    double *bptr, *pptr, *qptr ; /** FOR TRAVERSING INTERPOLATION BUFFER **/
    double  pp ;                 /** INVERSE TIMESTEP **/

    if ( ! ( bptr = (double *) baddr[ *fndx ][ *vndx ] ) )   /** NOT YET ALLOCATED **/
        {
        m3mesgc( "Error referencing internal buffer for DDTVAR3()" ) ;
        return( 0 ) ;
        }           /**  END IF-BLOCK:  NEED TO ALLOCATE THIS VARIABLE **/

    size = *bsize ;             /** = SIZEOF( TIME-STEP RECORD ) **/

    pptr = bptr + ( *last ? size : 0 ) ;
    qptr = bptr + ( *last ? 0 : size ) ;
    pp   = *p ;
    for  ( i = 0 ; i < size%4 ; i++ )
        {
        buffer[ i ] = pp * ( qptr[ i ]  -  pptr[ i ] ) ;
        }           /**  END FOR-LOOP ON I:  DDT FROM BUFFER **/
    for  (       ; i < size ; i+=4 )
        {
        buffer[ i   ] = pp * ( qptr[ i   ]  -  pptr[ i   ] ) ;
        buffer[ i+1 ] = pp * ( qptr[ i+1 ]  -  pptr[ i+1 ] ) ;
        buffer[ i+2 ] = pp * ( qptr[ i+2 ]  -  pptr[ i+2 ] ) ;
        buffer[ i+3 ] = pp * ( qptr[ i+3 ]  -  pptr[ i+3 ] ) ;
        }           /**  END FOR-LOOP ON I:  DDT FROM BUFFER **/

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION BUFDDT3D() **/



/*****************  BUFDEL3: *****************************************/
/** free up memory for SHUT3() and CLOSE3()  **/
    
void BUFDEL3 ( FINT * fndx )     /** M3 file index **/

    {  /** begin body of bufinit() **/
    int    i , j ;              /** LOOP COUNTERS AND SUBSCRIPTS **/

    i = *fndx ;
    for ( j = 0 ; j <= MXVARS3 ; j++ )
        {
        if ( baddr[ i ][ j ] )
            {
            free( (void*) baddr[ i ][ j ] ) ;
            baddr[ i ][ j ] = (FREAL *)0 ;
            }
        }                       /** END LOOP ON J **/

    }  /** end body of bufinit() **/

/*****************  BUFINTX: *****************************************/
/** Do time interpolation for INTERPX() **/
    
FINT BUFINTX ( FINT  *fndx,      /** M3 file index **/
               FINT  *vndx,      /** M3 variable index **/
               FINT  *bsize,     /** buffer size **/
               FINT  *last,      /**   " for p coeff in interpolation **/
               FINT  *tstep,     /** TRUE iff time-dependent file **/
               FINT   dims[3],   /** record-dimensions **/
               FINT   dim0[3],   /** hyperslab-starts **/
               FINT   dim1[3],   /** hyperslab-ends  **/
               FREAL *p,         /** interpolation coeff for last **/
               FREAL *q,         /** ... for (1 - last) **/
               FREAL *buffer )   /** output buffer array **/

    {  /**  begin body of bufintX() **/

    FINT   i, c, r, l ;         /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT   size, size1, size2 ; /** SCRATCH VARIABLES:  SLAB SIZES  **/
    FREAL  *aptr, *bptr ;       /** FOR TRAVERSING INTERPOLATION BUFFER **/
    FREAL  *pptr, *qptr ;       /** FOR TRAVERSING INTERPOLATION BUFFER **/
    FREAL  pp , qq ;            /** COEFFS AT P,Q **/

    if ( ! ( bptr = (FREAL *)baddr[ *fndx ][ *vndx ] ) )   /** NOT YET ALLOCATED **/
        {
        m3mesgc( "Error referencing internal buffer for INTERPX()" ) ;
        return( 0 ) ;
        }           /**  END IF-BLOCK:  NEED TO ALLOCATE THIS VARIABLE **/

    size  = *bsize ;             /** = SIZEOF( TIME-STEP RECORD ) **/
    size1 = dims[0] ;
    size2 = dims[0] * dims[1] ;

    if ( *tstep )                           /** IF TIME-STEPPED FILE **/
        {
        pp   = *p ;      /** BY CONSTRUCTION:  pp + qq = 1; pp,qq >= 0 **/
        qq   = *q ;
        if ( qq == 0.0 ) 
            {
            bptr = bptr + ( *last ? size : 0 ) ;
            for  ( i = 0 , l = dim0[2] - 1; l < dim1[2] ; l++ )
                {
                for  ( r = dim0[1] - 1; r < dim1[1] ; r++ )
                    {
                    pptr =  bptr + r * size1 + l * size2 + dim0[0] - 1 ;
                    for  ( c = dim0[0] - 1; c < dim1[0] ; c++, i++ )
                        {
                        buffer[ i ] = *pptr ;
                        *pptr++;
                        }
                    }
                }
            }                       /** IF qq==0 **/
        else if ( pp == 0.0 ) 
            {
            bptr = bptr + ( *last ? 0 : size ) ;
            for  ( i = 0 , l = dim0[2] - 1; l < dim1[2] ; l++ )
                {
                for  ( r = dim0[1] - 1; r < dim1[1] ; r++ )
                    {
                    pptr =  bptr + r * size1 + l * size2  + dim0[0] - 1;
                    for  ( c = dim0[0] - 1; c < dim1[0] ; c++, i++ )
                        {
                        buffer[ i ] = *pptr ;
                        *pptr++;
                        }
                    }
                }
            }                           /** IF  pp==0   **/
        else{                           /** ELSE pp,qq > 0  **/
            aptr = bptr + ( *last ? size : 0 ) ;
            bptr = bptr + ( *last ? 0 : size ) ;
            for  ( i = 0 , l = dim0[2] - 1; l < dim1[2] ; l++ )
                {
                for  ( r = dim0[1] - 1; r < dim1[1] ; r++ )
                    {
                    pptr =  aptr + r * size1 + l * size2 + dim0[0] - 1; ;
                    qptr =  bptr + r * size1 + l * size2 + dim0[0] - 1; ;
                    for  ( c = dim0[0] - 1; c < dim1[0] ; c++, i++ )
                        {
                        buffer[ i ] = pp * (*pptr) + qq * (*qptr) ;
                        *pptr++;
                        *qptr++;
                         }
                    }
                }
            }		        /** END IF:  qq==0, or pp==0, OR NOT **/
        }		        /** IF TIME STEPPED FILE **/

    else                                /** ELSE TIME-INDEPENDENT FILE **/
        {
        for  ( i = 0 , l = dim0[2] - 1; l < dim1[2] ; l++ )
            {
            for  ( r = dim0[1] - 1; r < dim1[1] ; r++ )
                {
                pptr =  bptr + r * size1 + l * size2 + dim0[0] - 1 ;
                for  ( c = dim0[0] - 1; c < dim1[0] ; c++, i++ )
                    {
                    buffer[ i ] = *pptr ;
                    *pptr++;
                    }
                }
            }
        }                       /** IF TIME-STEPPED FILE OR NOT **/

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION bufintX() **/


/*****************  BUFINTXD: *****************************************/
/** Do time interpolation for INTERP3() (type double) **/
    
FINT BUFINTXD( FINT   *fndx,      /** M3 file index **/
               FINT   *vndx,      /** M3 variable index **/
               FINT   *bsize,     /** buffer size **/
               FINT   *last,      /**   " for p coeff in interpolation **/
               FINT   *tstep,     /** TRUE iff time-dependent file **/
               FINT    dims[3],   /** record-dimensions **/
               FINT    dim0[3],   /** hyperslab-starts **/
               FINT    dim1[3],   /** hyperslab-ends  **/
               double *p,         /** interpolation coeff for last **/
               double *q,         /** ... for (1 - last) **/
               double *buffer )   /** output buffer array **/

    {  /**  begin body of bufintX() **/

    FINT    i, c, r, l ;         /** LOOP COUNTERS AND SUBSCRIPTS **/
    FINT    size, size1, size2 ; /** SCRATCH VARIABLES:  SLAB SIZES  **/
    double *aptr, *bptr ;       /** FOR TRAVERSING INTERPOLATION BUFFER **/
    double *pptr, *qptr ;       /** FOR TRAVERSING INTERPOLATION BUFFER **/
    double  pp , qq ;            /** COEFFS AT P,Q **/

    if ( ! ( bptr = (double *) baddr[ *fndx ][ *vndx ] ) )   /** NOT YET ALLOCATED **/
        {
        m3mesgc( "Error referencing internal buffer for INTERP3()" ) ;
        return( 0 ) ;
        }           /**  END IF-BLOCK:  NEED TO ALLOCATE THIS VARIABLE **/

    size  = *bsize ;             /** = SIZEOF( TIME-STEP RECORD ) **/
    size1 = dims[0] ;
    size2 = dims[0] * dims[1] ;

    if ( *tstep )                           /** IF TIME-STEPPED FILE **/
        {
        pp   = *p ;      /** BY CONSTRUCTION:  pp + qq = 1; pp,qq >= 0 **/
        qq   = *q ;
        if ( qq == 0.0 ) 
            {
            bptr = bptr + ( *last ? size : 0 ) ;
            for  ( i = 0 , l = dim0[2] - 1; l < dim1[2] ; l++ )
                {
                for  ( r = dim0[1] - 1; r < dim1[1] ; r++ )
                    {
                    pptr =  bptr + r * size1 + l * size2 + dim0[0] - 1 ;
                    for  ( c = dim0[0] - 1; c < dim1[0] ; c++, i++ )
                        {
                        buffer[ i ] = *pptr ;
                        *pptr++;
                        }
                    }
                }
            }                       /** IF qq==0 **/
        else if ( pp == 0.0 ) 
            {
            bptr = bptr + ( *last ? 0 : size ) ;
            for  ( i = 0 , l = dim0[2] - 1; l < dim1[2] ; l++ )
                {
                for  ( r = dim0[1] - 1; r < dim1[1] ; r++ )
                    {
                    pptr =  bptr + r * size1 + l * size2 + dim0[0] - 1 ;
                    for  ( c = dim0[0] - 1; c < dim1[0] ; c++, i++ )
                        {
                        buffer[ i ] = *pptr ;
                        *pptr++;
                        }
                    }
                }
            }                           /** IF  pp==0   **/
        else{                           /** ELSE pp,qq > 0  **/
            aptr = bptr + ( *last ? size : 0 ) ;
            bptr = bptr + ( *last ? 0 : size ) ;
            for  ( i = 0 , l = dim0[2] - 1; l < dim1[2] ; l++ )
                {
                for  ( r = dim0[1] - 1; r < dim1[1] ; r++ )
                    {
                    pptr =  aptr + r * size1 + l * size2 + dim0[0] - 1 ;
                    qptr =  bptr + r * size1 + l * size2 + dim0[0] - 1 ;
                    for  ( c = dim0[0] - 1; c < dim1[0] ; c++, i++ )
                        {
                        buffer[ i ] = pp * (*pptr) + qq * (*qptr) ;
                        *pptr++;
                        *qptr++;
                        }
                    }
                }
            }		        /** END IF:  qq==0, or pp==0, OR NOT **/
        }		        /** IF TIME STEPPED FILE **/

    else                                /** ELSE TIME-INDEPENDENT FILE **/
        {
        for  ( i = 0 , l = dim0[2] - 1; l < dim1[2] ; l++ )
            {
            for  ( r = dim0[1] - 1; r < dim1[1] ; r++ )
                {
                pptr =  bptr + r * size1 + l * size2 + dim0[0] - 1 ;
                for  ( c = dim0[0] - 1; c < dim1[0] ; c++, i++ )
                    {
                    buffer[ i ] = *pptr ;
                    *pptr++;
                    }
                }
            }
        }                       /** IF TIME-STEPPED FILE OR NOT **/

    return( -1 ) ;              /** .TRUE. **/

    }           /**  END FUNCTION bufintXD() **/


