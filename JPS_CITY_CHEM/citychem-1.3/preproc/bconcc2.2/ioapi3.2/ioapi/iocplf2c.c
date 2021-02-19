
/**********************************************************************
VERSION:
        EDSS/Models-3 I/O API -- Version 3
        "iocplf2c.c" version "$Id: iocplf2c.c 1 2017-06-10 18:05:20Z coats $"

COPYRIGHT
    (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    (C) 2003-2010  Baron Advanced Meteorological Systems.
    Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    See file "LGPL.txt" for conditions of use.

PURPOSE:
        Fortran bindings for I/O API Coupling Mode.

CALLS:
        Virtual I/O C-binding routines open3net(), shut3net(), desc3net(),
        read3net(), write3net()

REVISION HISTORY:
        Prototype  6/1998 by Atanas Trayanov, MCNC EMC.

        Modified 9/2004 by CJC for I/O API Version 3:  revised type
        system for Fortran coupling using types FINT, FREAL, FSTR_L
        defined in "parms3.h".  Updated MAX_STR_LEN to match POSIX.
        Should now work on LP64 systems...

        Modified 3/2005 by CJC to handle GNU-style double underscoring:  


        Modified 11/2005 by CJC:  extra name-mangling for Absoft Pro Fortran:
        upper-case Fortran  symbols, prepend _C to common blocks.
**********************************************************************/

#ifdef IOAPICPL

#include <string.h>
#include "parms3.h"
#include "fdesc3.h"

#if defined(SECOND_UNDERSCORE)

#define  shut3v_st_             shut3v_st__
#define  read3v_st_             read3v_st__
#define  write3v_st_            write3v_st__
#define  get_iocpl_version_     get_iocpl_version__
#define  get_pvm_version_       get_pvm_version__

#elif defined(FLDMN) || defined(_WIN32) || defined(ABSFT)

/**  do nothing  **/

#elif defined(__hpux) || defined(_AIX)

#define  shut3v_st_             shut3v_st
#define  read3v_st_             read3v_st
#define  write3v_st_            write3v_st
#define  get_iocpl_version_     get_iocpl_version
#define  get_pvm_version_       get_pvm_version

#else
 
#error   "Error compiling:  unsupported architecture"
 
#endif

#define MAX_STR_LEN 512

int desc3net(char *name, IOAPI_Bdesc3 *bdesc3p, IOAPI_Cdesc3 *cdesc3p) ;
int close3net(char *name) ;
int open3net(char *fname, 
	     IOAPI_Bdesc3 *bdesc3p, IOAPI_Cdesc3 *cdesc3p,
	     int status, char *pgm) ;
int read3net(char *fname, char *vname, int skip, 
	     int jdate, int jtime, void *buffer, int n, int buftype) ;
int shut3net() ;
int write3net(char *fname, char *vname, 
	      int jdate, int jtime, void *buffer, int n, int buftype) ;

/* =========================================================== */
/* =========================================================== */

IOAPI_Bdesc3 *get_bdesc3f()
{

  extern IOAPI_Bdesc3 bdesc3_;
  IOAPI_Bdesc3 *bdesc3p = &bdesc3_;

  return ( bdesc3p ) ;

}
/* =========================================================== */
IOAPI_Cdesc3 *get_cdesc3f()
{

  extern IOAPI_Cdesc3 cdesc3_;
  IOAPI_Cdesc3 *cdesc3p = &cdesc3_;

  return ( cdesc3p );

}
/* =========================================================== */
/* =========================================================== */
/*
static int f2cstring(char *cstr, char *fstr, int len)
{
  strncpy(cstr, fstr, len);
  cstr[len]='\0';

  return 1;
}
*/
#ifndef min
#define min(i,j) ((i)<(j)?(i):(j))
#endif


static int f2cstring(ds, ss, len)
    char *ds, *ss;      /* dst, src ptrs */
    int len;             /* src len */
{
    char *p;
    int dl = MAX_STR_LEN;

    for (p = ss + len; --p >= ss && *p == ' '; ) ;
    len = p - ss + 1;
    dl--;
    ds[0] = 0;
    if (len > dl)
        return 1;
    strncat(ds, ss, min(len, dl));
    return 0;
}

/* =========================================================== */

FINT open3v_ (char * fname_ptr, 
              FINT * mode,
              char * pgmname_ptr,
              FSTR_L fname_len,
              FSTR_L pgmname_len )
    {
    int filemode=*mode;
    char name[MAX_STR_LEN];
    char pgm[MAX_STR_LEN];
    IOAPI_Bdesc3 *get_bdesc3f();
    IOAPI_Cdesc3 *get_cdesc3f();
    IOAPI_Bdesc3 *bdesc3p;
    IOAPI_Cdesc3 *cdesc3p;
    char *trim;


    f2cstring(name, fname_ptr, fname_len);
    f2cstring(pgm, pgmname_ptr, pgmname_len);
    trim = strrchr(pgm,'/');

    bdesc3p=get_bdesc3f();
    cdesc3p=get_cdesc3f();

    return (FINT) open3net(name, bdesc3p, cdesc3p, filemode, (trim ? trim+1:pgm));
    }

/* =========================================================== */

FINT desc3v_ (char * fname_ptr, FSTR_L fname_len)
    {
    char name[MAX_STR_LEN];
    IOAPI_Bdesc3 *get_bdesc3f();
    IOAPI_Cdesc3 *get_cdesc3f();
    IOAPI_Bdesc3 *bdesc3p;
    IOAPI_Cdesc3 *cdesc3p;


    f2cstring(name, fname_ptr, fname_len);

    bdesc3p=get_bdesc3f();
    cdesc3p=get_cdesc3f();

    return desc3net(name, bdesc3p, cdesc3p);
    }

/* =========================================================== */
FINT shut3v_st_ ( void )
    {
    return (FINT) shut3net();
    }

/* =========================================================== */

FINT read3v_st_ ( char * fname_ptr,
                  char * vname_ptr,
                  FINT * iskip,
                  FINT * jdate,
                  FINT * jtime,
                  void * buffer,
                  FINT * icount,
                  FINT * ftype,
                  FSTR_L fname_len,
                  FSTR_L vname_len )
    {
    int skip=*iskip;
    int date=*jdate;
    int time=*jtime;
    int count=*icount;
    int type=*ftype;
    char file[MAX_STR_LEN], vble[MAX_STR_LEN];

    f2cstring(file, fname_ptr, fname_len);
    f2cstring(vble, vname_ptr, vname_len);

    return (FINT) read3net(file, vble, skip, date, time, buffer, count, type) ;
    }

/* =========================================================== */

FINT write3v_st_ (char * fname_ptr,
                  char * vname_ptr,
                  FINT * jdate,
                  FINT * jtime,
                  void * buffer,
                  FINT * icount,
                  FINT * ftype,
                  FSTR_L fname_len,
                  FSTR_L vname_len)
    {
    int date=*jdate;
    int time=*jtime;
    int count=*icount;
    int type=*ftype;
    char fnm[MAX_STR_LEN], vnm[MAX_STR_LEN];

    f2cstring(fnm, fname_ptr, fname_len);
    f2cstring(vnm, vname_ptr, vname_len);

    return (FINT) write3net(fnm,vnm,date,time,buffer,count,type);
    }

/* =========================================================== */

void get_iocpl_version_( char * vers_ptr, FSTR_L vers_len )
    {
    extern char *iocpl_version();
    char *version, *Ftrn_str;
    int i, len;

    Ftrn_str = vers_ptr; 
    len = vers_len;
    /* fill the Fortran string with blanks */
    for (i=0; i< len; i++)
        {
        Ftrn_str[i] = ' ';
        }

    version = iocpl_version();
    strncpy(Ftrn_str, version, min(len,STRLEN(version)));
    }

/* =========================================================== */

void get_pvm_version_( char * vers_ptr, FSTR_L vers_len )
    {
    extern char *pvm_version();
    char *version, *Ftrn_str;
    int i, len;

    Ftrn_str = vers_ptr; 
    len = vers_len;
    /* fill the Fortran string with blanks */
    for (i=0; i< len; i++)
        {
        Ftrn_str[i] = ' ';
        }

    version = pvm_version();
    strncpy(Ftrn_str, version, min(len,STRLEN(version)));
    }

/* =========================================================== */

void setsynchro3v_( FINT * newvalue )
   {
    int new_value=*newvalue;
    void setWaitForSynchro(int);

    setWaitForSynchro(new_value);
    }


/* =========================================================== */

#endif      /*  ifdef IOAPICPL   */
