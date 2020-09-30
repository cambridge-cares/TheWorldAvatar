
/********************************************************************
C  INCLUDE FILE  parms3.h
C
C    VERSION "$Id: parms3.h 96 2018-04-04 21:17:59Z coats $"
C    EDSS/Models-3 I/O API Version 3.1.
C       Copyright (C) 1992-2002 MCNC,
C       (C) 1992-2002,2005-2013  Carlie J. Coats, Jr.,
C       (C) 2003-2010 Baron AAdvanced Meteorological Systems, LLC. (BAMS), and
C       (C) 2015- UNC Institute for the Environment.
C       Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C       See file "LGPL.txt" for conditions of use.
C
C    DO NOT EDIT !!
C
C       The EDSS/Models-3 I/O API depends in an essential manner
C       upon the contents of this INCLUDE file.  ANY CHANGES are
C       likely to result in very obscure, difficult-to-diagnose
C       bugs caused by an inconsistency between standard "libioapi.a"
C       object-libraries and whatever code is compiled with the
C       resulting modified INCLUDE-file.
C
C       By making any changes to this INCLUDE file, the user
C       explicitly agrees that in the case any assistance is 
C       required of MCNC or of the I/O API author, CARLIE J. COATS, JR.
C       HE AND/OR HIS PROJECT OR CONTRACT AGREES TO REIMBURSE BAMS
C       AND/OR THE I/O API AUTHOR, CARLIE J. COATS, JR., AT A
C       RATE TRIPLE THE NORMAL CONTRACT RATE FOR THE SERVICES
C       REQUIRED.
C
C  CONTAINS:
C       dimensioning parameters, standard file-type, grid-type,
C       (etc.) token values for Models-3 I/O System API
C       Typedefs for "name" and "description" objects, and for
C       Fortran numeric types, at lines 234 &ff.
C
C  DEPENDENT UPON:
C       consistency with FORTRAN include-file PARMS3.EXT
C
C  REVISION HISTORY:
C       prototype 3/1992 by CJC
C       Modified 12/1992 by CJC:  new map projection type STEGRD3.
C       Modified  6/1994 by CJC:  I/O API Revisions.
C       Modified 12/1996 by CJC:  support for new file types
C	Modified  8/1999 by CJC:  FLDMN, Win32 portability enhancements
C	Modified  8/2003 by CJC:  INT_FC_STRLEN, STRLEN() for Intel "efc"
C	Modified 10/2003 by CJC:  for I/O API version 3:
C       move typedefs to "parms3.h" from "fdesc3.h"
C   Modified 02/2015 by CJC for I/O API version 3.2:  INTEGER*8 support
C   Modified 04/2015 by CJC:  SINUGRD3 map projection for MIMS
C       Spatial Allocator
************************************************************************/

#ifndef    PARMS3_DEFINED
#define    PARMS3_DEFINED

/**  FLDMN  indicates that Feldman-style Fortran bindings are required:
     Down-case and add trailing underscore to linker-visible objects
     Add an extra "length" argument passed by value for each character
     string argument, placed at the end of the argument list, in order
     of character-string arguments in the Fortran.
        **/
     
/**  FINT_T  is for user-customization on those platforms where a 
     Fortran INTEGER is not the same as a C "int" -- e.g., if you decide 
     to compile with "-i8" on those platforms which support it:
     FINT is the corresponding Fortran "INTEGER" type    **/
     
/**  REAL8  is for user-customization on those platforms where a 
     Fortran REAL is the same as a C double -- e.g., if you decide 
     to compile with "-r8" on those platforms which support it:
     FREAL is the corresponding Fortran "REAL" type  **/
     
/**  *FSTR_L  is for user-customization esp[ecially on those platforms
     where the C routine "strlen()" returns a non"int" size_t result,
     but the Fortran call interface expects an "int" as the length
     field for CHARACTER string arguments.
     This is subsequently used by macro  STRLEN( X )   **/
     
#if defined(__sgi)    || defined(__sun) || defined(__osf__) || \
    defined(__mips__) || defined(__OPENNT)
#define FLDMN 1
#endif

#if defined( FINT_T )
    typedef FINT_T FINT ;
#else
    typedef int    FINT ;
#endif              /** IF "FC" uses "int" for INTEGER **/

#if defined(_CRAYMPP) || defined(REAL8)          
   typedef double   FREAL ;
#else
   typedef float    FREAL ;
#endif

#if ! defined(FSTR_L)
#define    FSTR_L       int
#endif              /** IF "FC" uses "int" string-length arguments **/

#define    STRLEN( X )  ((FSTR_L)strlen( X ))


#ifdef __cplusplus
extern "C" {
#endif

                          /*  max number of open files                 */
#define   MXFILE3   (64)  

                          /*  max number of layers per file            */
#define   MXVARS3  (2048)  

                          /*  max number of additional TSRIES3 atts    */
#define   MXDESC3   (60)  

                          /*  description line (as FSTR) length        */
#define   MXLAYS3  (100)  

                          /*  max number of variables per file         */
#define   MXDLEN3   (80)  

                          /*  name length (as FSTR)                    */
#define   MXATTS3   (20)  

                          /*  max number of description lines          */
#define   NAMLEN3   (16)  


typedef char   M3Name[ NAMLEN3 ] ; /* Models3 Fortran "name" objects  */
typedef char   M3Line[ MXDLEN3 ] ; /* ... "description-line" objects  */


                          /*  file type value:  cloud-event files      */
#define   KFEVNT3   (-3)  

                          /*  file type value:  study-graph files      */
#define   DGRAPH3   (-2)  

                          /*  file type value:  custom files           */
#define   CUSTOM3   (-1)  

                          /*  file type value:  dictionary files       */
#define   DCTNRY3    (0)  

                          /*  file type value:  gridded files          */
#define   GRDDED3    (1)  

                          /*  file type value:  boundary files         */
#define   BNDARY3    (2)  

                          /*  file type value:  ID/scattered           */
#define   IDDATA3    (3)  

                          /*  file type value:  profile files          */
#define   PROFIL3    (4)  

                          /*  file type value:  grid-nest files        */
#define   GRNEST3    (5)  

                          /*  file type value:  sparse matrix files    */
#define   SMATRX3    (6)  

                          /*  file type value:  time-series files    */
#define   TSRIES3    (7)  

                          /*  file type value:  pointer-flyer files    */
#define   PTRFLY3    (8)  

                          /*  file type value:  MPI-distributed grid   */
#define   MPIGRD3    (9)  



                          /*  basic data type value:  byte (==INTEGER*1) */
#define   M3BYTE     (1)  

                          /*  basic data type value:  character-string */
#define   M3CHAR     (2)  

                          /*  basic data type value:  int */
#define   M3INT      (4)  

                          /*  basic data type value:  float */
#define   M3REAL     (5)  

                          /*  basic data type value:  double */
#define   M3DBLE     (6)  

                          /*  basic data type value:  int_64t */
#define   M3INT8     (10)  



                          /*  intraprocess-comm virtual files           */
#define   BUFFIL3   (-1)  

                          /*  interprocess-comm virtual files          */
#define   VIRFIL3   (-2)  

                          /*  file-sequences                           */
#define   LSTFIL3   (-3)  

                          /*  native-binary I/O files                  */
#define   BINFIL3   (-4)  


                          /*  open-status:  OLD-READ-ONLY              */
#define   FSREAD3    (1)  

                          /*  open-status:  OLD-READ/WRITE             */
#define   FSRDWR3    (2)  

                          /*  open-status:  NEW-READ/WRITE:  create    */
#define   FSNEW3     (3)  

                          /*  open-status:  unknown -- create if necessary*/
#define   FSUNKN3    (4)  

                          /*  open-status:  create if necessary; truncate */
#define   FSCREA3    (5)  


                          /*  grid type value:  lat-lon coords         */
#define   LATGRD3    (1)  
                          /*  grid type value:  Lambert coords         */
#define   LAMGRD3    (2)  
                          /*  grid type value:  Mercator coords        */
#define   MERGRD3    (3)  
                          /*  grid type value:  Stereographic coords   */
#define   STEGRD3    (4)  
                          /*  grid type value:  UTM coords             */
#define   UTMGRD3    (5)  
                          /*  grid type value:  polar stereographic coords */
#define   POLGRD3    (6)  
                          /*  grid type value:  equatorial Mercator coords */
#define   EQMGRD3    (7)  
                          /*  grid type value:  transverse Mercator coords */
#define   TRMGRD3    (8)  
                          /*  grid type value:  Albers conic Equal Area */
#define   ALBGRD3    (9)  
                          /*  grid type value:  Lambert Azimuthal Equal Area */
#define   LEQGRD3    (10)  
                          /*  grid type value:  Sinusoidal*/
#define   SINUGRD3   (11)

                          /*  vert coord type 1:  hydrostatic sigma-P  */
#define   VGSGPH3    (1)  
                          /*  vert coord type 2:  non-h sigma-P        */
#define   VGSGPN3    (2)  
                          /*  vert coord type 3:  sigma-Z              */
#define   VGSIGZ3    (3)  
                          /*  vert coord type 4:  pressure (mb)        */
#define   VGPRES3    (4)  
                          /*  vert coord type 5:  Z (m) (above sea lvl)*/
#define   VGZVAL3    (5)  
                          /*  vert coord type 6:  H (m) (above ground) */
#define   VGHVAL3    (6)  
                          /*  vert coord type 6:  WRF mass-core sigma  */
#define   VGWRFEM    (7)  
                          /*  vert coord type 6:  WRF NMM              */
#define   VGWRFNM    (8)  


                          /*  Flag value:  "good" values            */
#define   OKFLAG3   (5461)     

                          /*  Flag value:  read all layers, vbles   */
#define   ALLAYS3   (-1)     

                          /*  Flag value:  read all variables       */
#define   ALLVAR3  "ALL"     

                          /* float flag value: "bad" or "missing"   */ 
#define   BADVAL3   (-9.999E36)

                          /* BADVAL3 < AMISS3 on all machines       */
#define   AMISS3    (-9.000E36)  

                          /* int flag value: "bad" or "missing"     */
#define   IMISS3    (-9999)      

                          /* char string "bad" or "missing" */
#define   CMISS3  "????????????????" 

                          /* These match BSD-style "endian.h" values: */

#ifndef   LITTLE_ENDIAN
#define   LITTLE_ENDIAN	(1234)
#endif

#ifndef   BIG_ENDIAN
#define   BIG_ENDIAN	(4321)
#endif

#ifndef   PDP_ENDIAN
#define   PDP_ENDIAN	(3412)
#endif


#ifdef __cplusplus
}    /** END 'extern "c"' **/
#endif


#endif    /** PARMS3_DEFINED **/

/*****************   END   parms3.h   **********************************/


