
/************************************************************************
  INCLUDE FILE  attdsc.h
  VERSION "$Id: attdsc3.h 1 2017-06-10 18:05:20Z coats $"

EDSS/Models-3 I/O API.

COPYRIGHT
    (C) 1992-2002 MCNC,
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
       Hydrology time series file attribute typedefs and extern structs for 
       C bindings to the I/O API

  DEPENDENT UPON:  
       consistency with FORTRAN include-files PARMS33.EXT, ATTDSC3.EXT

  REVISION HISTORY:  
       prototype 12/30/96 by CJC

       Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
       upper-case Fortran  symbols, prepend _C to common blocks.

**************************************************************************/

#ifndef    ATTDSC3_DEFINED
#define    ATTDSC3_DEFINED

#include   "parms3.h"

/*************************************************************************      
        Typedef for a MODELS 3 attributes list:  same memory layout
        as the FORTRAN COMMONs BATTS3 and CATTS3.
**************************************************************************/

typedef struct { 
               FINT   natts[ MXVARS3 ] ;            /* # of atts per vble */
               FREAL  fatts[ MXVARS3 ][ MXATTS3 ] ; /* attribute values */
               }
               IOAPI_Batts3 ;

typedef struct { 
               M3Name  vname[ MXVARS3 ][ MXATTS3 ] ; /* attribute names */
               }
               IOAPI_Catts3 ;


#endif    /*   ATTDSC3_DEFINED  */

#if FLDMN || __sgi || __sun || __osf__ || __mips__
    
    extern IOAPI_Batts3 _batts3 ;
    extern IOAPI_Catts3 _catts3 ;

#elif __hpux || _AIX

    extern IOAPI_Batts3  batts3 ;
    extern IOAPI_Catts3  catts3 ;

#elif  ABSFT

    extern IOAPI_Batts3  _CBATTS3 ;
    extern IOAPI_Catts3  _CCATTS3 ;

#elif  _CRAY

    extern IOAPI_Batts3  BATTS3 ;
    extern IOAPI_Catts3  CATTS3 ;

#else
 
#error   "Error compiling:  unsupported architecture"
 
#endif              /** IF FELDMAN-DESCENDED F77 TARGETED, OR IF CRAY **/

/****************   END   attdsc3.h   ***********************************/


